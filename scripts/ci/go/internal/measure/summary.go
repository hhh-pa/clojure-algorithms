package measure

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/hhh-pa/clojure-algorithms/scripts/ci/go/internal/algocore"
)

// Summary is one parsed driver SUMMARY line: the clojure.test
// counts for a namespace run.
type Summary struct {
	Test  int64
	Pass  int64
	Fail  int64
	Error int64
}

var summaryKV = regexp.MustCompile(`:(test|pass|fail|error)\s+(\d+)`)

// ParseSummary extracts the driver's SUMMARY counts from child
// stdout. Namespaces print nondeterministically (timing, random
// samples, hash order), so the SUMMARY line — not full stdout —
// is the correctness gate. The last marker wins, so a test that
// echoes the marker cannot shadow the driver's line; the line
// itself must be exactly `SUMMARY {<four counts>}`.
func ParseSummary(text string) (Summary, error) {
	var sum Summary
	var ok bool
	for _, line := range strings.Split(text, "\n") {
		trimmed := strings.TrimSpace(line)
		if !strings.HasPrefix(trimmed, "SUMMARY ") {
			continue
		}
		body := strings.TrimSpace(strings.TrimPrefix(trimmed, "SUMMARY"))
		if !strings.HasPrefix(body, "{") || !strings.HasSuffix(body, "}") {
			continue
		}
		inner := body[1 : len(body)-1]
		found := map[string]int64{}
		for _, m := range summaryKV.FindAllStringSubmatch(inner, -1) {
			n, err := strconv.ParseInt(m[2], 10, 64)
			if err != nil {
				continue
			}
			found[m[1]] = n
		}
		if len(found) != 4 {
			continue
		}
		stripped := summaryKV.ReplaceAllString(inner, "")
		stripped = strings.ReplaceAll(stripped, ",", "")
		if strings.TrimSpace(stripped) != "" {
			continue
		}
		sum = Summary{Test: found["test"], Pass: found["pass"], Fail: found["fail"], Error: found["error"]}
		ok = true
	}
	if !ok {
		tail := strings.TrimSpace(text)
		if runes := []rune(tail); len(runes) > 200 {
			tail = string(runes[len(runes)-200:])
		}
		return Summary{}, fmt.Errorf("no SUMMARY line in: %s", algocore.PyRepr(tail))
	}
	return sum, nil
}

// CheckSummary gates one namespace run: failures or errors fail,
// anything else passes. Returns "" on success.
func CheckSummary(sum Summary) string {
	if sum.Fail > 0 || sum.Error > 0 {
		return fmt.Sprintf("namespace failed: :fail %d :error %d (of :test %d)", sum.Fail, sum.Error, sum.Test)
	}
	return ""
}

// summariesAgree compares two runs of the same namespace for the
// shim cross-check: same tests, no failures/errors either side,
// and assertions on both sides. :pass itself is excluded — two
// namespaces assert per edge of a random graph, so their tallies
// legitimately differ run to run. (Cross-runtime drift detection
// in report-compare stays strict with explicit exceptions; this
// check only guards counter-run divergence.)
func summariesAgree(a, b Summary) bool {
	return a.Test == b.Test && a.Fail == 0 && a.Error == 0 &&
		b.Fail == 0 && b.Error == 0 && a.Pass > 0 && b.Pass > 0
}
