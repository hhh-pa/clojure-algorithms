package measure

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"

	"github.com/hhh-pa/clojure-algorithms/scripts/ci/go/internal/algocore"
	"github.com/hhh-pa/clojure-algorithms/scripts/ci/go/internal/pyjson"
)

// ExpandTemplate fills the {cp}/{ns} holes (plus {{/}} escapes).
// Unknown holes and malformed templates are errors.
func ExpandTemplate(template, cp, ns string) (string, error) {
	holes := map[string]string{"cp": cp, "ns": ns}
	var sb strings.Builder
	i := 0
	for i < len(template) {
		c := template[i]
		switch c {
		case '{':
			if i+1 < len(template) && template[i+1] == '{' {
				sb.WriteByte('{')
				i += 2
				continue
			}
			end := strings.IndexByte(template[i:], '}')
			if end < 0 {
				return "", errors.New("Single '{' encountered in format string")
			}
			name := template[i+1 : i+end]
			if name == "" || strings.ContainsAny(name, "!:{") {
				return "", fmt.Errorf("unsupported hole {%s}", name)
			}
			value, ok := holes[name]
			if !ok {
				return "", fmt.Errorf("%s", algocore.PyRepr(name))
			}
			sb.WriteString(value)
			i += end + 1
		case '}':
			if i+1 < len(template) && template[i+1] == '}' {
				sb.WriteByte('}')
				i += 2
				continue
			}
			return "", errors.New("Single '}' encountered in format string")
		default:
			sb.WriteByte(c)
			i++
		}
	}
	return sb.String(), nil
}

// BuildInnerCommand resolves the driver argv for one namespace.
func BuildInnerCommand(template, cp, ns string) ([]string, error) {
	expanded, err := ExpandTemplate(template, cp, ns)
	if err != nil {
		return nil, fmt.Errorf("bad command template: %v", err)
	}
	argv, err := SplitCommand(expanded)
	if err != nil {
		return nil, fmt.Errorf("bad command template: %v", err)
	}
	if len(argv) == 0 {
		return nil, errors.New("bad command template: expands to empty argv")
	}
	return argv, nil
}

// AllocSupported reports whether template+ns resolve to a JVM argv
// (EpsilonGC allocation measurement is JVM-only).
func AllocSupported(template, cp, ns string) bool {
	inner, err := BuildInnerCommand(template, cp, ns)
	if err != nil || len(inner) == 0 {
		return false
	}
	return filepath.Base(inner[0]) == "java"
}

// ResolveTimeBin finds a GNU time binary.
func ResolveTimeBin(explicit string) (string, error) {
	path := explicit
	if path == "" {
		if _, err := os.Stat("/usr/bin/time"); err == nil {
			path = "/usr/bin/time"
		} else if found, err := exec.LookPath("time"); err == nil {
			path = found
		}
	}
	if path == "" {
		return "", errors.New("GNU time not found: install it (Debian/Ubuntu: apt install time) or pass --time-bin")
	}
	stdout, _, err := algocore.Run([]string{path, "--version"}, nil, 10)
	if err != nil {
		return "", fmt.Errorf("cannot execute time binary %s: %v", algocore.PyRepr(path), err)
	}
	if !strings.Contains(string(stdout), "GNU") {
		return "", fmt.Errorf("%s is not GNU time (need -f/-o support); install GNU time or pass --time-bin", algocore.PyRepr(path))
	}
	return path, nil
}

// PerfUsable reports whether perf counters run on this host.
func PerfUsable() bool {
	if _, err := exec.LookPath("perf"); err != nil {
		return false
	}
	tmp, err := os.CreateTemp("", "perf-probe-")
	if err != nil {
		return false
	}
	name := tmp.Name()
	tmp.Close()
	defer os.Remove(name)
	_, _, err = algocore.Run([]string{"perf", "stat", "-e", PerfEvents, "-x,", "-o", name, "--", "true"}, nil, 30)
	if err != nil {
		return false
	}
	raw, err := os.ReadFile(name)
	if err != nil {
		return false
	}
	counters := ParsePerfText(string(raw))
	return counters.Instructions != nil || counters.Cycles != nil
}

// BuildCommand wraps the driver directly (perf outside, time inside).
func BuildCommand(timeBin, timeFile string, usePerf bool, perfFile string, inner []string) []string {
	timed := append([]string{timeBin, "-f", TimeFormat, "-o", timeFile}, inner...)
	if usePerf {
		return append([]string{"perf", "stat", "-e", PerfEvents, "-x,", "-o", perfFile, "--"}, timed...)
	}
	return timed
}

// MeasureOnce runs the driver once under GNU time (+perf). The
// driver reads no stdin.
func MeasureOnce(timeBin string, usePerf bool, inner []string, timeout int) ([]byte, *RunMetrics, string) {
	timeFile, err := tempFile("algoperf-time-")
	if err != nil {
		return nil, nil, "time output unreadable: " + err.Error()
	}
	defer os.Remove(timeFile)
	var perfFile string
	if usePerf {
		perfFile, err = tempFile("algoperf-perf-")
		if err != nil {
			return nil, nil, "perf output unreadable: " + err.Error()
		}
		defer os.Remove(perfFile)
	}
	cmd := BuildCommand(timeBin, timeFile, usePerf, perfFile, inner)
	stdout, stderr, err := algocore.Run(cmd, nil, timeout)
	var timeoutErr *algocore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return nil, nil, fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		return nil, nil, fmt.Sprintf("exit %d: %s", algocore.ExitCode(err), algocore.LastLine(stderr))
	}
	timeRaw, err := os.ReadFile(timeFile)
	if err != nil {
		return nil, nil, "time output unreadable: " + err.Error()
	}
	parsed, err := ParseTimeText(string(timeRaw))
	if err != nil {
		return nil, nil, "time output unreadable: " + err.Error()
	}
	metrics := &RunMetrics{
		WallS: pyjson.FloatNum(parsed.WallS), UserS: pyjson.FloatNum(parsed.UserS),
		SysS: pyjson.FloatNum(parsed.SysS), CpuS: pyjson.FloatNum(parsed.CpuS),
		MaxRSSKb: parsed.MaxRSSKb, CpuPct: pyjson.FloatNum(parsed.CpuPct),
		MajorFaults: parsed.MajorFault, MinorFaults: parsed.MinorFault,
	}
	if usePerf {
		perfRaw, err := os.ReadFile(perfFile)
		if err != nil {
			return nil, nil, "perf output unreadable: " + err.Error()
		}
		counters := ParsePerfText(string(perfRaw))
		metrics.Instructions = counters.Instructions
		metrics.Cycles = counters.Cycles
	}
	return stdout, metrics, ""
}

// MeasureAllocOnce runs the driver once under EpsilonGC.
func MeasureAllocOnce(inner []string, timeout int) ([]byte, int64, string) {
	gcFile, err := tempFile("algoperf-gc-")
	if err != nil {
		return nil, 0, "epsilon log unreadable: " + err.Error()
	}
	defer os.Remove(gcFile)
	cmd := append([]string{inner[0]}, EpsilonFlags...)
	cmd = append(cmd, "-Xlog:gc:"+gcFile)
	cmd = append(cmd, inner[1:]...)
	stdout, stderr, err := algocore.Run(cmd, nil, timeout)
	var timeoutErr *algocore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return nil, 0, fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		errText := string(stderr)
		msg := fmt.Sprintf("exit %d: %s", algocore.ExitCode(err), algocore.LastLine(stderr))
		if strings.Contains(errText, "OutOfMemoryError") {
			msg += " (Epsilon heap exhausted; raise -Xmx in EPSILON_FLAGS)"
		}
		return nil, 0, msg
	}
	gcRaw, err := os.ReadFile(gcFile)
	if err != nil {
		return nil, 0, "epsilon log unreadable: " + err.Error()
	}
	used, err := ParseEpsilonUsed(string(gcRaw))
	if err != nil {
		return nil, 0, "epsilon log unreadable: " + err.Error()
	}
	return stdout, used, ""
}

// ResolveArgv0 finds the real binary behind PATH/symlinks: the
// /bin/java alternatives symlink loses libjli.so when launched
// through an instrumenting launcher (exit 127), so instrumented
// runs need the resolved path. Used for JVM and native legs
// alike.
func ResolveArgv0(argv0 string) string {
	path := argv0
	if !strings.Contains(path, string(os.PathSeparator)) {
		if found, err := exec.LookPath(path); err == nil {
			path = found
		}
	}
	if resolved, err := filepath.EvalSymlinks(path); err == nil {
		return resolved
	}
	return argv0
}

// InnerIsJava reports whether a resolved driver argv runs the JVM.
func InnerIsJava(inner []string) bool {
	return len(inner) > 0 && filepath.Base(inner[0]) == "java"
}

// TemplateHeadIsJava reports whether a command template's argv0 is
// the JVM.
func TemplateHeadIsJava(template string) bool {
	fields := strings.Fields(template)
	if len(fields) == 0 {
		return false
	}
	return filepath.Base(strings.Trim(fields[0], `"'`)) == "java"
}

// PinJVMFlags keeps HotSpot stable under Intel Pin: -Xrs stops
// the JVM from installing its full OS signal handlers (which
// would fight Pin's signal interception) and
// -XX:+AllowUserSignalHandlers lets Pin's own handlers coexist
// with the remaining JVM signal use (implicit null checks, thread
// coordination).
var PinJVMFlags = []string{"-Xrs", "-XX:+AllowUserSignalHandlers"}

// PinUsable reports whether Pin count runs can execute: the pin
// launcher resolves and the inscount tool file exists.
func PinUsable(pinBin, tool string) bool {
	if pinBin == "" || tool == "" {
		return false
	}
	if _, err := os.Stat(tool); err != nil {
		return false
	}
	if strings.Contains(pinBin, string(os.PathSeparator)) {
		info, err := os.Stat(pinBin)
		return err == nil && info.Mode()&0o111 != 0
	}
	_, err := exec.LookPath(pinBin)
	return err == nil
}

// PinVersion probes `pin -version` for report provenance, or nil
// when the launcher cannot answer. Compared runs must share one
// Pin version: counts are only comparable within a version.
func PinVersion(pinBin string) *string {
	stdout, _, err := algocore.Run([]string{pinBin, "-version"}, nil, 30)
	if err != nil {
		return nil
	}
	for _, line := range strings.Split(string(stdout), "\n") {
		if trimmed := strings.TrimSpace(line); trimmed != "" {
			return &trimmed
		}
	}
	return nil
}

// IsPinTimeout reports whether a PinOnce error text is a deadline
// timeout rather than a crash: timeouts mean the namespace
// exceeds the instrument's budget, so it degrades to a skip.
func IsPinTimeout(runErr string) bool {
	return strings.Contains(runErr, "TIMEOUT")
}

// pinLauncher prefixes the Pin command with setarch -R on Linux:
// HotSpot's heap reserve otherwise loses an ASLR lottery against
// Pin 4.4's mappings (mmap-overlap exit 127 on ~half the runs),
// while the fixed no-ASLR layout places them deterministically
// apart. Every compared leg runs the same fixed layout, so
// layout can never bias a comparison either way.
func pinLauncher(pinBin string) []string {
	if runtime.GOOS == "linux" {
		if setarch, err := exec.LookPath("setarch"); err == nil {
			return []string{setarch, "-R", pinBin}
		}
	}
	return []string{pinBin}
}

// PinNoASLRUsable reports whether JVM pin legs can fix the
// address layout (Linux setarch present; non-Linux has no Pin
// legs to protect).
func PinNoASLRUsable() bool {
	if runtime.GOOS != "linux" {
		return true
	}
	_, err := exec.LookPath("setarch")
	return err == nil
}

// PinOnce runs inner once under Intel Pin with the inscount tool
// and returns stdout plus the total instruction count,
// dispatching on the resolved argv0: the JVM recipe (below) for
// java, the plain recipe (no JVM flags) otherwise.
func PinOnce(pinBin, tool string, inner []string, timeout int) ([]byte, int64, string) {
	if !InnerIsJava(inner) {
		return nativePinOnce(pinBin, tool, inner, timeout)
	}
	return jvmPinOnce(pinBin, tool, inner, timeout)
}

// parsePinOutput reads the Count line from one tool run.
func parsePinOutput(outFile string) (int64, string) {
	raw, err := os.ReadFile(outFile)
	if err != nil {
		return 0, "pin output unreadable: " + err.Error()
	}
	count, err := ParsePinCount(string(raw))
	if err != nil {
		return 0, err.Error()
	}
	return count, ""
}

// jvmPinOnce runs a JVM argv once under Pin with PinJVMFlags
// injected right after the resolved java binary (JVM options must
// precede -D/-cp and the main class).
func jvmPinOnce(pinBin, tool string, inner []string, timeout int) ([]byte, int64, string) {
	outFile, err := tempFile("algoperf-pin-")
	if err != nil {
		return nil, 0, "pin output unreadable: " + err.Error()
	}
	defer os.Remove(outFile)
	cmd := pinLauncher(pinBin)
	cmd = append(cmd, "-t", tool, "-o", outFile, "--", ResolveArgv0(inner[0]))
	cmd = append(cmd, PinJVMFlags...)
	cmd = append(cmd, inner[1:]...)
	stdout, stderr, err := algocore.Run(cmd, nil, timeout)
	var timeoutErr *algocore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return nil, 0, fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		return nil, 0, fmt.Sprintf("exit %d: %s", algocore.ExitCode(err), algocore.LastLine(stderr))
	}
	count, runErr := parsePinOutput(outFile)
	if runErr != "" {
		return nil, 0, runErr
	}
	return stdout, count, ""
}

// nativePinOnce runs a native argv once under Pin with the
// inscount tool (same -o plumbing, no JVM flags).
func nativePinOnce(pinBin, tool string, inner []string, timeout int) ([]byte, int64, string) {
	outFile, err := tempFile("algoperf-pin-")
	if err != nil {
		return nil, 0, "pin output unreadable: " + err.Error()
	}
	defer os.Remove(outFile)
	cmd := []string{pinBin, "-t", tool, "-o", outFile, "--", ResolveArgv0(inner[0])}
	cmd = append(cmd, inner[1:]...)
	stdout, stderr, err := algocore.Run(cmd, nil, timeout)
	var timeoutErr *algocore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return nil, 0, fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		return nil, 0, fmt.Sprintf("exit %d: %s", algocore.ExitCode(err), algocore.LastLine(stderr))
	}
	count, runErr := parsePinOutput(outFile)
	if runErr != "" {
		return nil, 0, runErr
	}
	return stdout, count, ""
}

func tempFile(prefix string) (string, error) {
	fd, err := os.CreateTemp("", prefix)
	if err != nil {
		return "", err
	}
	name := fd.Name()
	fd.Close()
	return name, nil
}

// CaseOpts bundles MeasureCase inputs. Timeout budgets the
// native-speed runs (timed + alloc); PinTimeout budgets the Pin
// count phase, which degrades to a skip on expiry instead of
// failing.
type CaseOpts struct {
	TimeBin     string
	UsePerf     bool
	Template    string
	Classpath   string
	Label       string
	Stem        string
	Script      string
	Case        string
	Ns          string
	Repeat      int
	AllocRepeat int
	Timeout     int
	PinBin      string
	PinTool     string
	PinRepeat   int
	PinTimeout  int
	Root        string
}

// MeasureCase measures one namespace: timed runs, EpsilonGC
// allocation runs, then the Pin count phase. Every phase's first
// run passes the SUMMARY gate (missing SUMMARY or :fail/:error
// fails the namespace loudly).
func MeasureCase(o CaseOpts) CaseResult {
	res := CaseResult{
		Label: o.Label, Stem: o.Stem, Case: o.Case,
		Runs: []RunMetrics{}, AllocRuns: []int64{},
	}
	rel, err := filepath.Rel(o.Root, o.Script)
	if err != nil {
		rel = o.Script
	}
	res.Script = rel
	inner, err := BuildInnerCommand(o.Template, o.Classpath, o.Ns)
	if err != nil {
		msg := fmt.Sprintf("%s/%s: %v", o.Label, o.Case, err)
		res.Error = &msg
		return res
	}
	gate := func(phase string, stdout []byte) string {
		prefix := o.Label + "/" + o.Case
		if phase != "" {
			prefix = phase + ": " + prefix
		}
		sum, err := ParseSummary(string(stdout))
		if err != nil {
			return fmt.Sprintf("%s: %v", prefix, err)
		}
		if reason := CheckSummary(sum); reason != "" {
			return fmt.Sprintf("%s: %s", prefix, reason)
		}
		// Recorded from the first gated phase: the timed
		// runs when they exist, else the first instrument
		// phase (--repeat 0).
		if res.TestCounts == nil {
			res.TestCounts = &TestCounts{Test: sum.Test, Pass: sum.Pass, Fail: sum.Fail, Error: sum.Error}
		}
		return ""
	}
	for i := 0; i < o.Repeat; i++ {
		stdout, metrics, runErr := MeasureOnce(o.TimeBin, o.UsePerf, inner, o.Timeout)
		if runErr != "" {
			msg := fmt.Sprintf("%s/%s: %s", o.Label, o.Case, runErr)
			res.Error = &msg
			return res
		}
		if i == 0 {
			if msg := gate("", stdout); msg != "" {
				res.Error = &msg
				return res
			}
		}
		res.Runs = append(res.Runs, *metrics)
	}
	res.Summary = SummarizeCase(res.Runs)
	if res.Summary == nil && (o.AllocRepeat > 0 || o.PinRepeat > 0) {
		// --repeat 0 skips the timed phase; the instrument
		// phases still need a summary to hang their signals
		// on (Runs stays 0, which table/BMF renderers use to
		// tell pin-only results from timed ones).
		res.Summary = &CaseSummary{}
	}
	if o.AllocRepeat > 0 {
		for i := 0; i < o.AllocRepeat; i++ {
			stdout, used, runErr := MeasureAllocOnce(inner, o.Timeout)
			if runErr != "" {
				msg := fmt.Sprintf("%s/%s: alloc: %s", o.Label, o.Case, runErr)
				res.Error = &msg
				return res
			}
			if i == 0 {
				if msg := gate("alloc", stdout); msg != "" {
					res.Error = &msg
					return res
				}
			}
			res.AllocRuns = append(res.AllocRuns, used)
		}
		res.Summary.AllocatedBytes = SummarizeAlloc(res.AllocRuns)
	}
	if o.PinRepeat > 0 {
		var counts []int64
		for i := 0; i < o.PinRepeat; i++ {
			stdout, count, runErr := PinOnce(o.PinBin, o.PinTool, inner, o.PinTimeout)
			if runErr != "" {
				if IsPinTimeout(runErr) {
					skip := runErr
					res.PinSkipped = &skip
					fmt.Fprintf(os.Stderr, "warning: %s/%s/%s: pin skipped (%s)\n",
						o.Label, o.Stem, o.Case, runErr)
					break
				}
				msg := fmt.Sprintf("%s/%s: pin: %s", o.Label, o.Case, runErr)
				res.Error = &msg
				return res
			}
			if i == 0 {
				if msg := gate("pin", stdout); msg != "" {
					res.Error = &msg
					return res
				}
			}
			counts = append(counts, count)
		}
		if res.PinSkipped == nil {
			s := SummarizeInts(counts)
			res.Summary.PinInstructions = &s
		}
	}
	res.Ok = true
	return res
}

// SelectShard filters tasks deterministically by group: groups are
// sorted, and group i lands in shard i%total.
func SelectShard[T any](tasks []T, key func(T) string, index, total int) []T {
	groups := map[string][]T{}
	var order []string
	for _, t := range tasks {
		k := key(t)
		if _, seen := groups[k]; !seen {
			order = append(order, k)
		}
		groups[k] = append(groups[k], t)
	}
	sort.Strings(order)
	keep := map[string]bool{}
	for i, g := range order {
		if i%total == index {
			keep[g] = true
		}
	}
	var out []T
	for _, t := range tasks {
		if keep[key(t)] {
			out = append(out, t)
		}
	}
	return out
}
