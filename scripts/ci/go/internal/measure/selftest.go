package measure

import (
	"fmt"
	"io"
	"strings"

	"github.com/hhh-pa/clojure-algorithms/scripts/ci/go/internal/algocore"
)

// SelfTest runs the subprocess-free check suite (no JVM, time,
// perf, Pin, or C compiler): parser checks for every text format
// the sweep reads, template checks, and gate checks. Returns the
// process exit code (0 when all checks pass).
func SelfTest(out io.Writer) int {
	var checks []algocore.Check
	add := func(name string, ok bool, detail string) {
		checks = append(checks, algocore.Check{Name: name, Ok: ok, Detail: detail})
	}

	// SUMMARY gate.
	sum, err := ParseSummary("Testing x\nSUMMARY {:test 2, :pass 10, :fail 0, :error 0}\n")
	add("summary-valid", err == nil && sum == (Summary{Test: 2, Pass: 10}), fmt.Sprint(sum, err))
	_, err = ParseSummary("no marker here\n")
	add("summary-missing", err != nil, fmt.Sprint(err))
	_, err = ParseSummary("SUMMARY {:test 2, :pass x, :fail 0, :error 0}\n")
	add("summary-malformed", err != nil, fmt.Sprint(err))
	add("summary-gate-clean", CheckSummary(Summary{Test: 1, Pass: 1}) == "", "")
	add("summary-gate-fail", CheckSummary(Summary{Test: 1, Fail: 1}) != "", "")
	add("summary-gate-error", CheckSummary(Summary{Test: 1, Error: 1}) != "", "")

	// Pin count.
	count, err := ParsePinCount("Count 846018869\n")
	add("pin-valid", err == nil && count == 846018869, fmt.Sprint(count, err))
	_, err = ParsePinCount("no counts here\n")
	add("pin-missing", err != nil, fmt.Sprint(err))
	_, err = ParsePinCount("Count nope\n")
	add("pin-malformed", err != nil, fmt.Sprint(err))

	// EpsilonGC log.
	used, err := ParseEpsilonUsed("[0.1s] Heap: 12M reserved, 3M (25%) committed, 1536K (4%) used\n")
	add("epsilon-valid", err == nil && used == 1536*1024, fmt.Sprint(used, err))
	_, err = ParseEpsilonUsed("no heap line\n")
	add("epsilon-missing", err != nil, fmt.Sprint(err))

	// GNU time.
	tm, err := ParseTimeText("wall=1.5 user=1.2 sys=0.3 maxrss_kb=1024 cpu_pct=99% major_faults=1 minor_faults=2")
	add("time-valid", err == nil && tm.WallS == 1.5 && tm.CpuS == 1.5 && tm.MaxRSSKb == 1024, fmt.Sprint(tm, err))
	_, err = ParseTimeText("wall=1.5")
	add("time-truncated", err != nil, fmt.Sprint(err))

	// Templates.
	argv, err := BuildInnerCommand(DefaultCommandTemplate, "a.jar:b", "foo.bar-test")
	want := []string{"java", "-Duser.language=en", "-Duser.country=US", "-cp", "a.jar:b", "clojure.main", "perf/driver.clj", "foo.bar-test"}
	add("template-jvm", err == nil && equalStrs(argv, want), fmt.Sprint(argv, err))
	argv, err = BuildInnerCommand("clojure-rs run perf/driver.clj {ns} src test", "", "foo.bar-test")
	want = []string{"clojure-rs", "run", "perf/driver.clj", "foo.bar-test", "src", "test"}
	add("template-native", err == nil && equalStrs(argv, want), fmt.Sprint(argv, err))
	_, err = BuildInnerCommand("java -cp {bogus} x", "", "n")
	add("template-bad-hole", err != nil, fmt.Sprint(err))

	// Leg detection.
	add("leg-java", InnerIsJava([]string{"java", "-cp", "x"}) && InnerIsJava([]string{"/usr/bin/java"}), "")
	add("leg-native", !InnerIsJava([]string{"clojure-rs", "run"}) && !InnerIsJava(nil), "")
	add("leg-template", TemplateHeadIsJava(DefaultCommandTemplate) && !TemplateHeadIsJava("clojure-rs run {ns}"), "")

	failed := 0
	for _, c := range checks {
		status := "ok"
		if !c.Ok {
			status = "FAIL"
			failed++
		}
		if c.Detail != "" {
			fmt.Fprintf(out, "self-test [%s] %s: %s\n", status, c.Name, strings.TrimSpace(c.Detail))
		} else {
			fmt.Fprintf(out, "self-test [%s] %s\n", status, c.Name)
		}
	}
	fmt.Fprintf(out, "self-test: %d failed, %d passed\n", failed, len(checks)-failed)
	if failed > 0 {
		return 1
	}
	return 0
}

func equalStrs(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
