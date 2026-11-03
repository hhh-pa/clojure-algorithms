package measure

import (
	"bytes"
	"os/exec"
	"runtime"
	"strings"
	"testing"

	"github.com/hhh-pa/clojure-algorithms/scripts/ci/go/internal/algocore"
)

// TestSelfTestGreen runs the full CLI self-test suite in-process:
// every check must pass with no JVM, time, perf, or valgrind.
func TestSelfTestGreen(t *testing.T) {
	var buf bytes.Buffer
	if code := SelfTest(&buf); code != 0 {
		t.Fatalf("SelfTest exit %d:\n%s", code, buf.String())
	}
	if !strings.Contains(buf.String(), "0 failed") {
		t.Fatalf("missing summary:\n%s", buf.String())
	}
}

// TestAllocShimLive builds the counting shim and runs a shell under
// it: stdout must pass through intact and the total must parse.
// Linux-only (LD_PRELOAD/glibc).
func TestAllocShimLive(t *testing.T) {
	if runtime.GOOS != "linux" {
		t.Skip("alloc shim needs Linux (LD_PRELOAD)")
	}
	if _, err := exec.LookPath("cc"); err != nil {
		t.Skip("needs cc to build the shim")
	}
	so, err := EnsureAllocShim()
	if err != nil {
		t.Fatalf("build shim: %v", err)
	}
	stdout, stderr, err := algocore.RunEnv(
		[]string{"sh", "-c", "echo hi"}, nil, 60,
		[]string{"LD_PRELOAD=" + so})
	if err != nil {
		t.Fatalf("run under shim: %v\n%s", err, stderr)
	}
	if string(stdout) != "hi\n" {
		t.Fatalf("stdout mangled: %q", stdout)
	}
	total, err := ParseAllocShimTotal(string(stderr))
	if err != nil {
		t.Fatalf("parse total: %v\n%s", err, stderr)
	}
	if total <= 0 {
		t.Fatalf("non-positive total: %d", total)
	}
}
