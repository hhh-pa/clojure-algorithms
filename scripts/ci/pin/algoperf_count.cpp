// algoperf_count: Pin tool for the algoperf sweep.
//
// Counts dynamic instructions (all threads) plus cumulative
// malloc-family requested bytes, printing exactly two lines:
//
//   Count <total instructions>
//   Alloc <total bytes>
//
// The Go backend (internal/measure) parses those lines into
// pin_instructions / pin_allocated_bytes. Both lines always
// print, even when zero, so a run without libc malloc still
// parses.
//
// Instruction counting is per-thread (TLS) with a lock-held
// sum at thread exit: no atomics on the hot path, exact
// under the JVM's many threads. Allocation counting mirrors
// the LD_PRELOAD shim: malloc/calloc/realloc/reallocarray/
// memalign/posix_memalign/aligned_alloc requests summed, only
// when the call succeeds, freed bytes never subtracted.
// BEFORE captures the size, AFTER adds it; nested family
// calls (reallocarray -> realloc) count once because AFTER
// clears the pending slot.

#include <fstream>
#include <iostream>

#include "pin.H"

KNOB< std::string > KnobOutputFile(KNOB_MODE_WRITEONCE, "pintool", "o", "algoperf_count.out", "specify output file name");

static std::ofstream OutFile;
static TLS_KEY tls_key = INVALID_TLS_KEY;
static PIN_LOCK totals_lock;
static UINT64 total_ins = 0;
static UINT64 total_alloc = 0;

class thread_data_t
{
  public:
    thread_data_t() : _ins(0), _alloc(0), _pending(0) {}
    UINT64 _ins;
    UINT64 _alloc;
    UINT64 _pending;
};

static thread_data_t* GetTData(THREADID threadid)
{
    return static_cast< thread_data_t* >(PIN_GetThreadData(tls_key, threadid));
}

// Per-basic-block instruction count (hot path: one TLS lookup
// plus an add, no locks).
VOID PIN_FAST_ANALYSIS_CALL Docount(UINT32 c, THREADID threadid) { GetTData(threadid)->_ins += c; }

VOID ThreadStart(THREADID threadid, CONTEXT* ctxt, INT32 flags, VOID* v)
{
    thread_data_t* tdata = new thread_data_t;
    if (PIN_SetThreadData(tls_key, tdata, threadid) == FALSE)
    {
        std::cerr << "PIN_SetThreadData failed" << std::endl;
        PIN_ExitProcess(1);
    }
}

VOID ThreadFini(THREADID threadid, const CONTEXT* ctxt, INT32 code, VOID* v)
{
    thread_data_t* tdata = GetTData(threadid);
    PIN_GetLock(&totals_lock, threadid + 1);
    total_ins += tdata->_ins;
    total_alloc += tdata->_alloc;
    PIN_ReleaseLock(&totals_lock);
    delete tdata;
}

VOID Trace(TRACE trace, VOID* v)
{
    for (BBL bbl = TRACE_BblHead(trace); BBL_Valid(bbl); bbl = BBL_Next(bbl))
    {
        BBL_InsertCall(bbl, IPOINT_ANYWHERE, (AFUNPTR)Docount, IARG_FAST_ANALYSIS_CALL, IARG_UINT32, BBL_NumIns(bbl),
                       IARG_THREAD_ID, IARG_END);
    }
}

// Single-size allocators: malloc(size), realloc(ptr, size),
// memalign(align, size), aligned_alloc(align, size).
VOID SizeBefore(THREADID threadid, ADDRINT size) { GetTData(threadid)->_pending = static_cast< UINT64 >(size); }

// Product allocators: calloc(n, size), reallocarray(ptr, n, size).
VOID ProdBefore(THREADID threadid, ADDRINT n, ADDRINT size)
{
    GetTData(threadid)->_pending = static_cast< UINT64 >(n) * static_cast< UINT64 >(size);
}

// posix_memalign(memptr, align, size).
VOID PMABefore(THREADID threadid, ADDRINT memptr, ADDRINT align, ADDRINT size)
{
    GetTData(threadid)->_pending = static_cast< UINT64 >(size);
}

// Pointer-returning allocators: count only on success.
VOID RetAfter(THREADID threadid, ADDRINT ret)
{
    thread_data_t* tdata = GetTData(threadid);
    if (ret != 0) tdata->_alloc += tdata->_pending;
    tdata->_pending = 0;
}

// posix_memalign returns an error code: count only on zero.
VOID ErrAfter(THREADID threadid, ADDRINT err)
{
    thread_data_t* tdata = GetTData(threadid);
    if (err == 0) tdata->_alloc += tdata->_pending;
    tdata->_pending = 0;
}

static VOID InstrumentSize(IMG img, const char* name, UINT32 size_arg)
{
    RTN rtn = RTN_FindByName(img, name);
    if (!RTN_Valid(rtn)) return;
    RTN_Open(rtn);
    RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR)SizeBefore, IARG_THREAD_ID, IARG_FUNCARG_ENTRYPOINT_VALUE, size_arg,
                   IARG_END);
    RTN_InsertCall(rtn, IPOINT_AFTER, (AFUNPTR)RetAfter, IARG_THREAD_ID, IARG_FUNCRET_EXITPOINT_VALUE, IARG_END);
    RTN_Close(rtn);
}

static VOID InstrumentProd(IMG img, const char* name, UINT32 n_arg, UINT32 size_arg)
{
    RTN rtn = RTN_FindByName(img, name);
    if (!RTN_Valid(rtn)) return;
    RTN_Open(rtn);
    RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR)ProdBefore, IARG_THREAD_ID, IARG_FUNCARG_ENTRYPOINT_VALUE, n_arg,
                   IARG_FUNCARG_ENTRYPOINT_VALUE, size_arg, IARG_END);
    RTN_InsertCall(rtn, IPOINT_AFTER, (AFUNPTR)RetAfter, IARG_THREAD_ID, IARG_FUNCRET_EXITPOINT_VALUE, IARG_END);
    RTN_Close(rtn);
}

static VOID InstrumentPMA(IMG img)
{
    RTN rtn = RTN_FindByName(img, "posix_memalign");
    if (!RTN_Valid(rtn)) return;
    RTN_Open(rtn);
    RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR)PMABefore, IARG_THREAD_ID, IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                   IARG_FUNCARG_ENTRYPOINT_VALUE, 1, IARG_FUNCARG_ENTRYPOINT_VALUE, 2, IARG_END);
    RTN_InsertCall(rtn, IPOINT_AFTER, (AFUNPTR)ErrAfter, IARG_THREAD_ID, IARG_FUNCRET_EXITPOINT_VALUE, IARG_END);
    RTN_Close(rtn);
}

VOID Image(IMG img, VOID* v)
{
    InstrumentSize(img, "malloc", 0);
    InstrumentSize(img, "realloc", 1);
    InstrumentSize(img, "memalign", 1);
    InstrumentSize(img, "aligned_alloc", 1);
    InstrumentProd(img, "calloc", 0, 1);
    InstrumentProd(img, "reallocarray", 1, 2);
    InstrumentPMA(img);
}

VOID Fini(INT32 code, VOID* v)
{
    OutFile << "Count " << total_ins << std::endl;
    OutFile << "Alloc " << total_alloc << std::endl;
    OutFile.close();
}

INT32 Usage()
{
    std::cerr << "Counts dynamic instructions and malloc-family bytes for algoperf" << std::endl;
    std::cerr << std::endl << KNOB_BASE::StringKnobSummary() << std::endl;
    return -1;
}

int main(int argc, char* argv[])
{
    PIN_InitSymbols();
    if (PIN_Init(argc, argv)) return Usage();

    tls_key = PIN_CreateThreadDataKey(0);
    if (tls_key == INVALID_TLS_KEY)
    {
        std::cerr << "PIN_CreateThreadDataKey failed" << std::endl;
        PIN_ExitProcess(1);
    }

    OutFile.open(KnobOutputFile.Value().c_str());

    TRACE_AddInstrumentFunction(Trace, 0);
    IMG_AddInstrumentFunction(Image, 0);
    PIN_AddThreadStartFunction(ThreadStart, 0);
    PIN_AddThreadFiniFunction(ThreadFini, 0);
    PIN_AddFiniFunction(Fini, 0);

    PIN_StartProgram();

    return 0;
}
