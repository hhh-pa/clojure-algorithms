package measure

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

	"github.com/hhh-pa/clojure-algorithms/scripts/ci/go/internal/pyjson"
)

// PrintTable prints one line per result on out.
func PrintTable(out io.Writer, results []CaseResult, failures []string) {
	for _, res := range results {
		tag := res.Label + "/" + res.Stem + "/" + res.Case
		if !res.Ok {
			errText := ""
			if res.Error != nil {
				errText = *res.Error
			}
			fmt.Fprintf(out, "FAIL %s\n", errText)
			continue
		}
		if res.Skipped != nil {
			fmt.Fprintf(out, "%s: skipped (%s)\n", tag, *res.Skipped)
			continue
		}
		s := res.Summary
		if s == nil {
			fmt.Fprintf(out, "%s: ok no timed runs\n", tag)
			continue
		}
		if s.Runs == 0 {
			// Pin-only (--repeat 0): no timed signals, so
			// report the instrument counts instead of zeros.
			var parts []string
			if s.PinInstructions != nil {
				parts = append(parts, fmt.Sprintf("pin_min=%s pin_med=%s pin_max=%s",
					numPlain(s.PinInstructions.Min), numPlain(s.PinInstructions.Median), numPlain(s.PinInstructions.Max)))
			}
			if s.CallgrindInstructions != nil {
				parts = append(parts, fmt.Sprintf("cg_min=%s cg_med=%s cg_max=%s",
					numPlain(s.CallgrindInstructions.Min), numPlain(s.CallgrindInstructions.Median), numPlain(s.CallgrindInstructions.Max)))
			}
			if len(parts) == 0 {
				parts = append(parts, "no timed runs")
			}
			fmt.Fprintf(out, "%s: ok %s\n", tag, strings.Join(parts, " "))
			continue
		}
		insn := "None"
		if s.Instructions != nil {
			insn = numPlain(s.Instructions.Median)
		}
		alloc := "n/a"
		if s.AllocatedBytes != nil {
			alloc = fmt.Sprintf("%dB", s.AllocatedBytes.Min)
		}
		fmt.Fprintf(out, "%s: ok cpu_min=%.3fs cpu_med=%.3fs rss_max=%dK wall_min=%.3fs insn_med=%s alloc_min=%s\n",
			tag, s.CpuS.Min.Float64(), s.CpuS.Median.Float64(),
			s.MaxRSSKb.Max.I, s.WallS.Min.Float64(), insn, alloc)
	}
	ok := 0
	for _, r := range results {
		if r.Ok {
			ok++
		}
	}
	fmt.Fprintf(out, "measure: %d ok, %d failed\n", ok, len(failures))
}

// numPlain renders a Num the way Python str() renders its int/float:
// bare digits for ints, repr for floats (11.0 keeps its .0).
func numPlain(n pyjson.Num) string {
	if n.IsInt {
		return strconv.FormatInt(n.I, 10)
	}
	return pyjson.FloatRepr(n.F)
}

// AppendSummary appends the Markdown results table, with the
// conditional cg_insn / cg_alloc columns (present only when some
// benchmark reports callgrind data).
func AppendSummary(path string, report Report, bmf BMF) error {
	showCg := false
	showCgAlloc := false
	for _, m := range bmf {
		if m.CallgrindInstructions != nil {
			showCg = true
		}
		if m.CallgrindAllocatedBytes != nil {
			showCgAlloc = true
		}
	}
	header := "| namespace | cpu total (s) | peak RSS (MB) | alloc total (MB) | instructions |"
	sep := "| --- | ---: | ---: | ---: | ---: |"
	if showCg {
		header += " cg_insn |"
		sep += " ---: |"
	}
	if showCgAlloc {
		header += " cg_alloc (MB) |"
		sep += " ---: |"
	}
	header += " wall total (s) |"
	sep += " ---: |"
	cpuModel := "unknown cpu"
	if report.Host.CpuModel != nil {
		cpuModel = *report.Host.CpuModel
	}
	javaVersion := "unknown"
	if report.Runtime.JavaVersion != nil {
		javaVersion = *report.Runtime.JavaVersion
	}
	perfUsed := "no"
	if report.Runtime.PerfUsed {
		perfUsed = "yes"
	}
	allocRepeat := "n/a"
	if report.Config.AllocRepeat != nil {
		allocRepeat = fmt.Sprintf("%d", *report.Config.AllocRepeat)
	}
	runtimeVersion := ""
	if report.Runtime.RuntimeVersion != nil {
		runtimeVersion = fmt.Sprintf(", runtime: %s", *report.Runtime.RuntimeVersion)
	}
	ok := 0
	skipped := 0
	for _, r := range report.Results {
		if r.Skipped != nil {
			skipped++
		} else if r.Ok {
			ok++
		}
	}
	skippedNote := ""
	if skipped > 0 {
		skippedNote = fmt.Sprintf(", %d skipped", skipped)
	}
	lines := []string{
		"## Perf measurement (`algoperf measure`)",
		"",
		fmt.Sprintf("- %s (%s), java: %s%s", report.TimestampUTC, cpuModel, javaVersion, runtimeVersion),
		fmt.Sprintf("- runtime: %s, perf counters: %s, repeat: %d, jobs: %d, alloc_repeat: %s",
			report.Runtime.Name, perfUsed, report.Config.Repeat, report.Config.Jobs, allocRepeat),
		fmt.Sprintf("- %d ok%s, %d failed", ok, skippedNote, len(report.Failures)),
		"- Primary signals: min CPU time, max peak RSS, min allocated " +
			"bytes, median instructions (load-independent); wall time is " +
			"secondary. Compare same-machine ratios only.",
		"",
		header,
		sep,
	}
	perNsWall := map[string]float64{}
	perNsInsn := map[string][]*pyjson.Num{}
	for _, res := range report.Results {
		if !res.Ok || res.Skipped != nil || res.Summary == nil {
			continue
		}
		key := res.Label
		perNsWall[key] += res.Summary.WallS.Median.Float64()
		if res.Summary.Instructions != nil {
			median := res.Summary.Instructions.Median
			perNsInsn[key] = append(perNsInsn[key], &median)
		} else {
			perNsInsn[key] = append(perNsInsn[key], nil)
		}
	}
	for _, name := range SortedNames(bmf) {
		m := bmf[name]
		if m.CpuTime == nil || m.PeakMemory == nil {
			panic(fmt.Sprintf("measure: BMF entry %q has no cpu_time/peak_memory; cannot render summary", name))
		}
		parts := perNsInsn[name]
		insn := "n/a"
		if len(parts) > 0 {
			var nums []pyjson.Num
			complete := true
			for _, p := range parts {
				if p == nil {
					complete = false
					break
				}
				nums = append(nums, *p)
			}
			if complete {
				insn = numPlain(pyjson.Sum(nums))
			}
		}
		alloc := "n/a"
		if m.AllocatedBytes != nil {
			alloc = fmt.Sprintf("%.1f", m.AllocatedBytes.Value.Float64()/1048576)
		}
		row := fmt.Sprintf("| %s | %.3f | %.1f | %s | %s |",
			name,
			m.CpuTime.Value.Float64(), m.PeakMemory.Value.Float64()/1048576,
			alloc, insn)
		if showCg {
			cg := "n/a"
			if m.CallgrindInstructions != nil {
				cg = numPlain(m.CallgrindInstructions.Value)
			}
			row += fmt.Sprintf(" %s |", cg)
		}
		if showCgAlloc {
			cga := "n/a"
			if m.CallgrindAllocatedBytes != nil {
				cga = fmt.Sprintf("%.1f", m.CallgrindAllocatedBytes.Value.Float64()/1048576)
			}
			row += fmt.Sprintf(" %s |", cga)
		}
		row += fmt.Sprintf(" %.3f |", perNsWall[name])
		lines = append(lines, row)
	}
	lines = append(lines, "")
	fd, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o666)
	if err != nil {
		return fmt.Errorf("cannot append summary to %s: %v", path, err)
	}
	defer fd.Close()
	_, err = fd.WriteString(strings.Join(lines, "\n"))
	return err
}
