// Package measure runs the algorithms perf sweep: one driver run
// per test namespace under GNU time (+perf), plus the Pin
// instruction-count backend. Ported from the snippet repo's
// measure package (recipe-identical JVM/BMF); the snippet case
// discovery and full-stdout gating are replaced by namespace
// discovery and the SUMMARY gate (see summary.go).
package measure

// TimeFormat is the GNU time -f layout (TIME_FORMAT).
const TimeFormat = "wall=%e user=%U sys=%S maxrss_kb=%M cpu_pct=%P major_faults=%F minor_faults=%R"

// PerfEvents are the user-space counters (PERF_EVENTS).
const PerfEvents = "instructions:u,cycles:u"

// ToolVersion tags the report; 2 adds the pin_instructions
// signal plus pin_version provenance.
const ToolVersion = 2

// DefaultCommandTemplate runs one namespace on the JVM. The
// locale is pinned: several namespaces format doubles with
// `format` (locale-sensitive), so a comma-decimal host locale
// would fail assertions and perturb instruction counts.
const DefaultCommandTemplate = "java -Duser.language=en -Duser.country=US -cp {cp} clojure.main perf/driver.clj {ns}"

// EpsilonFlags make heap-used-at-exit equal total allocation.
var EpsilonFlags = []string{
	"-XX:+UnlockExperimentalVMOptions",
	"-XX:+UseEpsilonGC",
	"-Xms2g",
	"-Xmx2g",
}

// AllocRepeatDefault mirrors the snippet default.
const AllocRepeatDefault = 3

// PinRepeatDefault runs two Pin counts per namespace: native
// counts agree to ~0.1% and JVM counts to a few percent, enough
// to catch instrument flakiness without paying for more runs.
const PinRepeatDefault = 2
