// algoperf_count: Pin tool for the algoperf sweep.
//
// Counts dynamic instructions (all threads), printing one line:
//
//   Count <total instructions>
//
// The Go backend (internal/measure) parses that line into
// pin_instructions. Memory is benchmarked via RSS, not Pin:
// allocation tracing would add probe cost without feeding any
// reported signal.
//
// Counting is per-thread (TLS) with a lock-held sum at thread
// exit: no atomics on the hot path, exact under the JVM's many
// threads.

#include <fstream>
#include <iostream>

#include "pin.H"

KNOB< std::string > KnobOutputFile(KNOB_MODE_WRITEONCE, "pintool", "o", "algoperf_count.out", "specify output file name");

static std::ofstream OutFile;
static TLS_KEY tls_key = INVALID_TLS_KEY;
static PIN_LOCK totals_lock;
static UINT64 total_ins = 0;

class thread_data_t
{
  public:
    thread_data_t() : _ins(0) {}
    UINT64 _ins;
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

VOID Fini(INT32 code, VOID* v)
{
    OutFile << "Count " << total_ins << std::endl;
    OutFile.close();
}

INT32 Usage()
{
    std::cerr << "Counts dynamic instructions for algoperf" << std::endl;
    std::cerr << std::endl << KNOB_BASE::StringKnobSummary() << std::endl;
    return -1;
}

int main(int argc, char* argv[])
{
    if (PIN_Init(argc, argv)) return Usage();

    tls_key = PIN_CreateThreadDataKey(0);
    if (tls_key == INVALID_TLS_KEY)
    {
        std::cerr << "PIN_CreateThreadDataKey failed" << std::endl;
        PIN_ExitProcess(1);
    }

    OutFile.open(KnobOutputFile.Value().c_str());

    TRACE_AddInstrumentFunction(Trace, 0);
    PIN_AddThreadStartFunction(ThreadStart, 0);
    PIN_AddThreadFiniFunction(ThreadFini, 0);
    PIN_AddFiniFunction(Fini, 0);

    PIN_StartProgram();

    return 0;
}
