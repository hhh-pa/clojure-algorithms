package measure

import (
	"testing"
)

// The SUMMARY gate is new in algoperf (namespaces print
// nondeterministically, so full-stdout comparison cannot work).
// TDD: these checks first, implementation in summary.go.
func TestParseSummary(t *testing.T) {
	sum, err := ParseSummary("Testing foo\n\nRan 2 tests containing 10 assertions.\n0 failures, 0 errors.\nSUMMARY {:test 2, :pass 10, :fail 0, :error 0}\n")
	if err != nil {
		t.Fatalf("parse valid: %v", err)
	}
	if sum.Test != 2 || sum.Pass != 10 || sum.Fail != 0 || sum.Error != 0 {
		t.Fatalf("wrong counts: %+v", sum)
	}
}

func TestParseSummaryAmidNoise(t *testing.T) {
	// Surrounding noise is fine; the last marker wins (a test
	// echoing the marker cannot shadow the driver's line).
	out := "random line\nSUMMARY {:test 9, :pass 9, :fail 0, :error 0}\nmore noise\nSUMMARY {:test 1, :pass 3, :fail 0, :error 0}\n"
	sum, err := ParseSummary(out)
	if err != nil {
		t.Fatalf("parse amid noise: %v", err)
	}
	if sum.Test != 1 || sum.Pass != 3 {
		t.Fatalf("wrong counts: %+v", sum)
	}
}

func TestParseSummaryMissing(t *testing.T) {
	if _, err := ParseSummary("Testing foo\nboom\n"); err == nil {
		t.Fatal("missing SUMMARY must error")
	}
}

func TestParseSummaryMalformed(t *testing.T) {
	for _, bad := range []string{
		"SUMMARY {:test 2, :pass x, :fail 0, :error 0}\n",
		"SUMMARY {:test 2}\n",
		"SUMMARY not-a-map\n",
		"SUMMARY {:test -1, :pass 0, :fail 0, :error 0}\n",
		"SUMMARY {:test 1, :pass 3, :fail 0, :error 0} trailing\n",
	} {
		if _, err := ParseSummary(bad); err == nil {
			t.Fatalf("malformed SUMMARY must error: %q", bad)
		}
	}
}

func TestCheckSummary(t *testing.T) {
	ok := Summary{Test: 2, Pass: 10}
	if err := CheckSummary(ok); err != "" {
		t.Fatalf("clean summary must pass: %q", err)
	}
	if err := CheckSummary(Summary{Test: 2, Pass: 9, Fail: 1}); err == "" {
		t.Fatal("failures must fail the gate")
	}
	if err := CheckSummary(Summary{Test: 2, Pass: 9, Error: 1}); err == "" {
		t.Fatal("errors must fail the gate")
	}
}
