package algocore

import (
	"testing"
)

func TestParseMeasureShard(t *testing.T) {
	idx, total, err := ParseMeasureShard("0/8")
	if err != nil || idx != 0 || total != 8 {
		t.Fatalf("parse 0/8: %d %d %v", idx, total, err)
	}
	idx, total, err = ParseMeasureShard("7/8")
	if err != nil || idx != 7 || total != 8 {
		t.Fatalf("parse 7/8: %d %d %v", idx, total, err)
	}
	for _, bad := range []string{"", "8/8", "0/0", "x/y", "1"} {
		if _, _, err := ParseMeasureShard(bad); err == nil {
			t.Fatalf("shard %q must error", bad)
		}
	}
}

func TestTemplateNeedsClasspath(t *testing.T) {
	if !TemplateNeedsClasspath("java -cp {cp} clojure.main {ns}") {
		t.Fatal("{cp} hole not detected")
	}
	if TemplateNeedsClasspath("clojure-rs run perf/driver.clj {ns} src test") {
		t.Fatal("false positive on native template")
	}
	if TemplateNeedsClasspath("echo {{cp}}") {
		t.Fatal("escaped hole must not count")
	}
}
