package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"github.com/hhh-pa/clojure-algorithms/scripts/ci/go/internal/algocore"
	"github.com/hhh-pa/clojure-algorithms/scripts/ci/go/internal/measure"
	"github.com/hhh-pa/clojure-algorithms/scripts/ci/go/internal/pyjson"
)

const measureUsage = `Load-independent performance measurement for the algorithms repo.

Runs perf/driver.clj once per test namespace and records cpu_s,
maxrss_kb, allocated_bytes, instructions/cycles, plus (under the
new flags) callgrind instruction counts and Pin instruction counts.
--repeat 0 skips the timed phase and runs only the instrument
phases (pin-only legs).

Usage: algoperf measure [--cp CP] [--repeat N] [--jobs N] [--timeout S]
                        [--only SUBSTR] [--output F] [--bmf-output F]
                        [--summary-file F] [--time-bin BIN] [--no-perf]
                        [--runtime NAME] [--command-template T] [--shard K/N]
                        [--alloc-repeat N | --no-alloc]
                        [--callgrind] [--callgrind-repeat N]
                        [--callgrind-timeout S]
                        [--pin] [--pin-bin BIN] [--pin-tool SO]
                        [--pin-repeat N] [--pin-timeout S]

       algoperf measure --merge INPUT... [--output F] [--bmf-output F]
                        [--summary-file F]

       algoperf measure --self-test
`

func runMeasure(argv []string) int {
	root := algocore.RepoRoot()
	var cp, only, output, bmfOutput, summaryFile, timeBin, shard, pinBin, pinTool string
	runtimeName := "jvm"
	template := measure.DefaultCommandTemplate
	jobs := 1
	timeout := 1800
	repeat := 1
	allocRepeat := measure.AllocRepeatDefault
	callgrindRepeat := measure.CallgrindRepeatDefault
	callgrindTimeout := 600
	pinRepeat := measure.PinRepeatDefault
	pinTimeout := 600
	perf := true
	alloc := true
	merge := false
	selfTest := false
	callgrind := false
	pin := false
	positionals, err := parseFlags("measure", argv, flagSpec{
		Strs: map[string]*string{
			"cp": &cp, "only": &only,
			"output": &output, "bmf-output": &bmfOutput,
			"summary-file": &summaryFile, "time-bin": &timeBin,
			"runtime": &runtimeName, "command-template": &template,
			"shard": &shard, "pin-bin": &pinBin, "pin-tool": &pinTool,
		},
		Ints: map[string]*int{
			"jobs": &jobs, "timeout": &timeout, "repeat": &repeat,
			"alloc-repeat":      &allocRepeat,
			"callgrind-repeat":  &callgrindRepeat,
			"callgrind-timeout": &callgrindTimeout,
			"pin-repeat":        &pinRepeat,
			"pin-timeout":       &pinTimeout,
		},
		Bools: map[string]*bool{
			"perf": &perf, "alloc": &alloc, "merge": &merge,
			"self-test": &selfTest, "callgrind": &callgrind, "pin": &pin,
		},
	})
	if err != nil {
		if errors.Is(err, errHelp) {
			fmt.Print(measureUsage)
			return 0
		}
		fmt.Fprintln(os.Stderr, err)
		return 2
	}
	if selfTest {
		return measure.SelfTest(os.Stdout)
	}
	if merge {
		return runMerge(positionals, output, bmfOutput, summaryFile)
	}
	if repeat < 0 {
		fmt.Fprintln(os.Stderr, "algoperf measure: --repeat must be >= 0")
		return 2
	}
	if allocRepeat < 1 {
		fmt.Fprintln(os.Stderr, "algoperf measure: --alloc-repeat must be >= 1")
		return 2
	}
	if jobs < 1 {
		fmt.Fprintln(os.Stderr, "algoperf measure: --jobs must be >= 1")
		return 2
	}
	if callgrindRepeat < 1 {
		fmt.Fprintln(os.Stderr, "algoperf measure: --callgrind-repeat must be >= 1")
		return 2
	}
	if callgrindTimeout < 1 {
		fmt.Fprintln(os.Stderr, "algoperf measure: --callgrind-timeout must be >= 1")
		return 2
	}
	if pinRepeat < 1 {
		fmt.Fprintln(os.Stderr, "algoperf measure: --pin-repeat must be >= 1")
		return 2
	}
	if pinTimeout < 1 {
		fmt.Fprintln(os.Stderr, "algoperf measure: --pin-timeout must be >= 1")
		return 2
	}
	var shardParsed *[2]int
	if shard != "" {
		idx, total, err := algocore.ParseMeasureShard(shard)
		if err != nil {
			fmt.Fprintln(os.Stderr, "algoperf measure:", err)
			return 2
		}
		shardParsed = &[2]int{idx, total}
	}

	timeBinPath, err := measure.ResolveTimeBin(timeBin)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	usePerf := perf && measure.PerfUsable()
	if perf && !usePerf {
		fmt.Fprintln(os.Stderr, "warning: perf counters unavailable; instructions/cycles will be null")
	}
	useCallgrind := callgrind && measure.CallgrindUsable()
	if callgrind && !useCallgrind {
		fmt.Fprintln(os.Stderr, "warning: valgrind unavailable; callgrind_instructions will be null")
	}
	usePin := pin && measure.PinUsable(pinBin, pinTool)
	if pin && !usePin {
		fmt.Fprintln(os.Stderr, "warning: pin unavailable (need --pin-bin launcher and --pin-tool inscount .so); pin_instructions will be null")
	}
	var pinVersion *string
	if usePin {
		pinVersion = measure.PinVersion(pinBin)
	}
	classpath, err := algocore.ResolveClasspath(root, cp)
	if err != nil {
		if !algocore.TemplateNeedsClasspath(template) {
			fmt.Fprintln(os.Stderr, "warning: no Clojure classpath; continuing without one (template has no {cp} hole)")
			classpath = ""
		} else {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	found, err := DiscoverNamespaces(root)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	skips, err := LoadSkips(root, found)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}

	type task struct {
		ns, stem string
	}
	var tasks []task
	var skipped []measure.CaseResult
	driver := filepath.Join(root, "perf", "driver.clj")
	for _, entry := range found {
		if only != "" && !strings.Contains(entry.Ns, only) {
			continue
		}
		if reason, excluded := skips[entry.Ns]; excluded {
			skipped = append(skipped, measure.SkippedResult(entry.Ns, entry.Stem, "perf/driver.clj", "run", reason))
			continue
		}
		tasks = append(tasks, task{entry.Ns, entry.Stem})
	}
	if shardParsed != nil {
		tasks = measure.SelectShard(tasks, func(t task) string {
			return t.ns
		}, shardParsed[0], shardParsed[1])
	}

	effectiveAlloc := 0
	if alloc {
		effectiveAlloc = allocRepeat
	}
	if effectiveAlloc > 0 && len(tasks) > 0 {
		t0 := tasks[0]
		if !measure.AllocSupported(template, classpath, t0.ns) {
			fmt.Fprintln(os.Stderr, "warning: allocation measurement needs a java command template; skipping (BMF will omit allocated_bytes)")
			effectiveAlloc = 0
		}
	}
	// Callgrind runs for JVM and native legs alike: the recipe
	// branches on the resolved argv0 (EpsilonGC log vs LD_PRELOAD
	// counting). Only valgrind's absence disables it.
	effectiveCg := 0
	if useCallgrind {
		effectiveCg = callgrindRepeat
	}
	effectivePin := 0
	if usePin {
		effectivePin = pinRepeat
	}
	if repeat == 0 && effectiveAlloc == 0 && effectiveCg == 0 && effectivePin == 0 {
		fmt.Fprintln(os.Stderr, "algoperf measure: --repeat 0 needs a measurement phase (--alloc, --callgrind, or --pin)")
		return 2
	}

	results := make([]measure.CaseResult, len(tasks))
	work := make(chan int)
	total := len(tasks)
	var done atomic.Int64
	var printMu sync.Mutex
	var workers sync.WaitGroup
	for w := 0; w < jobs; w++ {
		workers.Add(1)
		go func() {
			defer workers.Done()
			for i := range work {
				t := tasks[i]
				results[i] = measure.MeasureCase(measure.CaseOpts{
					TimeBin: timeBinPath, UsePerf: usePerf, Template: template,
					Classpath: classpath, Label: t.ns, Stem: t.stem,
					Script: driver, Case: "run", Ns: t.ns, Repeat: repeat,
					AllocRepeat: effectiveAlloc, CallgrindRepeat: effectiveCg,
					Timeout: timeout, CallgrindTimeout: callgrindTimeout, Root: root,
					PinBin: pinBin, PinTool: pinTool, PinRepeat: effectivePin,
					PinTimeout: pinTimeout,
				})
				n := done.Add(1)
				printMu.Lock()
				fmt.Println(formatCaseProgress(int(n), total, results[i]))
				printMu.Unlock()
			}
		}()
	}
	go func() {
		for i := range tasks {
			work <- i
		}
		close(work)
	}()
	workers.Wait()
	for _, s := range skipped {
		fmt.Printf("measure [skip] %s/%s/%s: %s\n", s.Label, s.Stem, s.Case, *s.Skipped)
	}
	results = append(results, skipped...)
	sort.Slice(results, func(i, j int) bool {
		if results[i].Label != results[j].Label {
			return results[i].Label < results[j].Label
		}
		if results[i].Stem != results[j].Stem {
			return results[i].Stem < results[j].Stem
		}
		return results[i].Case < results[j].Case
	})
	failures := []string{}
	for _, r := range results {
		if !r.Ok && r.Error != nil {
			failures = append(failures, *r.Error)
		}
	}
	sort.Strings(failures)

	javaLeg := false
	nativeBin := ""
	if len(tasks) > 0 {
		t0 := tasks[0]
		if inner, err := measure.BuildInnerCommand(template, classpath, t0.ns); err == nil && len(inner) > 0 {
			if measure.InnerIsJava(inner) {
				javaLeg = true
			} else {
				nativeBin = inner[0]
			}
		}
	} else {
		javaLeg = measure.TemplateHeadIsJava(template)
	}
	var javaVersion, runtimeVersion *string
	if javaLeg {
		javaVersion = measure.JavaVersion()
	} else if nativeBin != "" {
		runtimeVersion = measure.RuntimeVersion(nativeBin)
	}
	bmf := measure.BuildBMF(results)
	report := measure.Report{
		Tool: "algoperf measure", Version: measure.ToolVersion,
		TimestampUTC: time.Now().UTC().Format("2006-01-02T15:04:05.000000-07:00"),
		Host:         measure.HostInfo(),
		Runtime: measure.Runtime{
			Name: runtimeName, CommandTemplate: template,
			JavaVersion: javaVersion, RuntimeVersion: runtimeVersion,
			Classpath: classpath,
			TimeBin:   timeBinPath, PerfUsed: usePerf, CallgrindUsed: effectiveCg > 0,
			PinUsed: effectivePin > 0, PinVersion: pinVersion,
		},
		Config: measure.Config{
			Repeat: repeat, Jobs: jobs, Timeout: timeout, Only: only,
			Shard: strOrNil(shard), AllocRepeat: &effectiveAlloc,
			CallgrindRepeat: effectiveCg, CallgrindTimeout: callgrindTimeout,
			PinRepeat: effectivePin, PinTimeout: pinTimeout,
		},
		Results: results, Failures: failures,
	}
	if output != "" {
		if err := writeJSON(output, report); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	if bmfOutput != "" {
		if err := writeJSON(bmfOutput, bmf); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	if summaryFile != "" {
		if err := measure.AppendSummary(summaryFile, report, bmf); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	measure.PrintTable(os.Stdout, results, failures)
	if len(failures) > 0 {
		return 1
	}
	return 0
}

func strOrNil(s string) *string {
	if s == "" {
		return nil
	}
	return &s
}

// formatCaseProgress renders one live progress line for a finished
// namespace.
func formatCaseProgress(done, total int, res measure.CaseResult) string {
	outcome := "ok"
	if !res.Ok {
		outcome = "failed"
		if res.Error != nil {
			outcome = *res.Error
		}
	} else if res.CallgrindSkipped != nil {
		outcome = "ok (callgrind skipped: " + *res.CallgrindSkipped + ")"
	} else if res.PinSkipped != nil {
		outcome = "ok (pin skipped: " + *res.PinSkipped + ")"
	}
	return fmt.Sprintf("measure [%d/%d] %s/%s/%s: %s",
		done, total, res.Label, res.Stem, res.Case, outcome)
}

func runMerge(inputs []string, output, bmfOutput, summaryFile string) int {
	if len(inputs) == 0 {
		fmt.Fprintln(os.Stderr, "algoperf measure: --merge needs at least one input report")
		return 2
	}
	report, bmf, err := measure.MergeReports(inputs)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	if output != "" {
		if err := writeJSON(output, report); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	if bmfOutput != "" {
		if err := writeJSON(bmfOutput, bmf); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	if summaryFile != "" {
		if err := measure.AppendSummary(summaryFile, report, bmf); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
	}
	fmt.Printf("merge: %d results, %d benchmarks, %d failures\n",
		len(report.Results), len(bmf), len(report.Failures))
	if len(report.Failures) > 0 {
		return 1
	}
	return 0
}

func writeJSON(path string, v any) error {
	raw, err := pyjson.MarshalIndent2(v)
	if err != nil {
		return fmt.Errorf("cannot write %s: %v", path, err)
	}
	if err := os.WriteFile(path, raw, 0o644); err != nil {
		return fmt.Errorf("cannot write %s: %v", path, err)
	}
	return nil
}
