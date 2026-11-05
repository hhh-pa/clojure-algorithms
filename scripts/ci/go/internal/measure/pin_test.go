package measure

import (
	"bytes"
	"strings"
	"testing"

	"github.com/hhh-pa/clojure-algorithms/scripts/ci/go/internal/pyjson"
)

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
