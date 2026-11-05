package measure

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/hhh-pa/clojure-algorithms/scripts/ci/go/internal/pyjson"
)

func TestPinLauncher(t *testing.T) {
	got := pinLauncher("/opt/pin/pin")
	if got[len(got)-1] != "/opt/pin/pin" {
		t.Fatalf("launcher must end with the pin binary: %q", got)
	}
	if runtime.GOOS == "linux" {
		if _, err := exec.LookPath("setarch"); err != nil {
			if len(got) != 1 {
				t.Fatalf("without setarch must run pin directly: %q", got)
			}
		} else if len(got) != 3 || got[1] != "-R" || !strings.HasSuffix(got[0], "setarch") {
			t.Fatalf("with setarch must prefix [setarch -R]: %q", got)
		}
		if !PinNoASLRUsable() {
			t.Fatal("PinNoASLRUsable must hold where setarch exists")
		}
	} else if len(got) != 1 {
		t.Fatalf("non-linux must run pin directly: %q", got)
	}
}

func TestParsePinCount(t *testing.T) {
	if n, err := ParsePinCount("Count 846018869\n"); err != nil || n != 846018869 {
		t.Fatalf("single line: got %d, %v", n, err)
	}
	if n, err := ParsePinCount("Pin 3.31\nCount 1\nCount 42\n"); err != nil || n != 42 {
		t.Fatalf("later Count must win: got %d, %v", n, err)
	}
	for _, bad := range []string{"", "no counts here\n", "Count abc\n", "Count\n", "Count 1 2\n"} {
		if n, err := ParsePinCount(bad); err == nil {
			t.Fatalf("must reject %q: got %d", bad, n)
		}
	}
}

func TestPinUsableMissing(t *testing.T) {
	if PinUsable("", "") {
		t.Fatal("empty pin paths must be unusable")
	}
	if PinUsable("/nonexistent/pin", "/nonexistent/tool.so") {
		t.Fatal("missing pin files must be unusable")
	}
}

func TestPinOnlySummary(t *testing.T) {
	pin := MinMedMax{Min: pyjson.IntNum(100), Median: pyjson.IntNum(150), Max: pyjson.IntNum(200)}
	res := CaseResult{Label: "l", Stem: "s", Case: "run", Ok: true,
		Summary: &CaseSummary{Runs: 0, PinInstructions: &pin}}
	var buf bytes.Buffer
	PrintTable(&buf, []CaseResult{res}, nil)
	if !strings.Contains(buf.String(), "l/s/run: ok pin_min=100 pin_med=150 pin_max=200") {
		t.Fatalf("pin-only table line wrong:\n%s", buf.String())
	}
	if bmf := BuildBMF([]CaseResult{res}); len(bmf) != 0 {
		t.Fatalf("pin-only results must stay out of BMF: %+v", bmf)
	}
}

func TestBuildBMFPin(t *testing.T) {
	pin := MinMedMax{Min: pyjson.IntNum(100), Median: pyjson.IntNum(150), Max: pyjson.IntNum(200)}
	timed := func() CaseResult {
		return CaseResult{Label: "l", Stem: "s", Case: "run", Ok: true,
			Summary: &CaseSummary{
				Runs:            1,
				CpuS:            MinMedMax{Min: pyjson.FloatNum(1), Median: pyjson.FloatNum(1), Max: pyjson.FloatNum(1)},
				MaxRSSKb:        MinMedMax{Min: pyjson.IntNum(8), Median: pyjson.IntNum(8), Max: pyjson.IntNum(8)},
				WallS:           MinMedMax{Min: pyjson.FloatNum(1), Median: pyjson.FloatNum(1), Max: pyjson.FloatNum(1)},
				PinInstructions: &pin,
			}}
	}
	bmf := BuildBMF([]CaseResult{timed()})
	bench, ok := bmf["l"]
	if !ok {
		t.Fatal("timed result with pin counts must land in BMF")
	}
	if bench.PinInstructions == nil || bench.PinInstructions.Value != pyjson.IntNum(100) {
		t.Fatalf("pin_instructions must use the min as point estimate: %+v", bench.PinInstructions)
	}
	if bench.PinInstructions.UpperValue != pyjson.IntNum(200) {
		t.Fatalf("pin_instructions upper must be the max: %+v", bench.PinInstructions)
	}
	// Without pin counts the field stays absent (omitempty keeps
	// old reports byte-identical).
	res := timed()
	res.Summary.PinInstructions = nil
	bench = BuildBMF([]CaseResult{res})["l"]
	if bench.PinInstructions != nil {
		t.Fatalf("pin_instructions must be absent without pin counts: %+v", bench)
	}
}

func TestAppendSummaryPinColumns(t *testing.T) {
	pin := MinMedMax{Min: pyjson.IntNum(100), Median: pyjson.IntNum(150), Max: pyjson.IntNum(200)}
	results := []CaseResult{{Label: "l", Stem: "s", Case: "run", Ok: true,
		Summary: &CaseSummary{
			Runs:            1,
			CpuS:            MinMedMax{Min: pyjson.FloatNum(1), Median: pyjson.FloatNum(1), Max: pyjson.FloatNum(1)},
			MaxRSSKb:        MinMedMax{Min: pyjson.IntNum(8), Median: pyjson.IntNum(8), Max: pyjson.IntNum(8)},
			WallS:           MinMedMax{Min: pyjson.FloatNum(2), Median: pyjson.FloatNum(2), Max: pyjson.FloatNum(2)},
			PinInstructions: &pin,
		}}}
	report := Report{
		Tool: "algoperf measure", TimestampUTC: "t",
		Host:    Host{CpuModel: strptr("cpu")},
		Runtime: Runtime{Name: "jvm"},
		Config:  Config{Repeat: 1, Jobs: 1},
		Results: results,
	}
	path := filepath.Join(t.TempDir(), "summary.md")
	if err := AppendSummary(path, report, BuildBMF(results)); err != nil {
		t.Fatalf("AppendSummary: %v", err)
	}
	raw, _ := os.ReadFile(path)
	text := string(raw)
	for _, want := range []string{"pin_insn", "| l |", " 100 |"} {
		if !strings.Contains(text, want) {
			t.Fatalf("summary must contain %q:\n%s", want, text)
		}
	}
	if strings.Contains(text, "pin_alloc") {
		t.Fatalf("summary must not contain a pin_alloc column:\n%s", text)
	}
}

func strptr(s string) *string { return &s }

func TestIsPinTimeout(t *testing.T) {
	if !IsPinTimeout("TIMEOUT (>900s)") {
		t.Fatal("timeout not detected")
	}
	for _, other := range []string{"", "exit 1: boom", "pin output unreadable: x", "pin count unreadable: x"} {
		if IsPinTimeout(other) {
			t.Fatalf("false positive: %q", other)
		}
	}
}
