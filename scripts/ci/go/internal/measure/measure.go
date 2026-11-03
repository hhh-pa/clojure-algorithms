// Package measure runs the algorithms perf sweep: one driver run
// per test namespace under GNU time (+perf), plus the gungraun-style
// callgrind instruction backend. Ported from the snippet repo's
// measure package (recipe-identical valgrind/JVM/shim/BMF); the
// snippet case discovery and full-stdout gating are replaced by
// namespace discovery and the SUMMARY gate (see summary.go).
package measure

// TimeFormat is the GNU time -f layout (TIME_FORMAT).
const TimeFormat = "wall=%e user=%U sys=%S maxrss_kb=%M cpu_pct=%P major_faults=%F minor_faults=%R"

// PerfEvents are the user-space counters (PERF_EVENTS).
const PerfEvents = "instructions:u,cycles:u"

// ToolVersion tags the report.
const ToolVersion = 1

// DefaultCommandTemplate runs one namespace on the JVM.
const DefaultCommandTemplate = "java -cp {cp} clojure.main perf/driver.clj {ns}"

// EpsilonFlags make heap-used-at-exit equal total allocation.
var EpsilonFlags = []string{
	"-XX:+UnlockExperimentalVMOptions",
	"-XX:+UseEpsilonGC",
	"-Xms2g",
	"-Xmx2g",
}

// AllocRepeatDefault mirrors the snippet default.
const AllocRepeatDefault = 3

// CallgrindRepeatDefault mirrors the one-shot gungraun default.
const CallgrindRepeatDefault = 1
