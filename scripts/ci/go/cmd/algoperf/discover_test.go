package main

import (
	"os"
	"path/filepath"
	"testing"
)

func TestDiscoverNamespaces(t *testing.T) {
	root := t.TempDir()
	write := func(rel, ns string) {
		p := filepath.Join(root, "test", rel)
		if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
			t.Fatal(err)
		}
		body := "(ns " + ns + "\n  (:require [clojure.test :refer [deftest]]))\n"
		if err := os.WriteFile(p, []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	write("clojure_algorithms/searches/linear_search_test.clj",
		"clojure-algorithms.searches.linear-search-test")
	write("clojure_algorithms/core_test.clj", "clojure-algorithms.core-test")
	// Non-test files are ignored.
	other := filepath.Join(root, "test", "clojure_algorithms", "helper.clj")
	if err := os.WriteFile(other, []byte("(ns helper)\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	found, err := DiscoverNamespaces(root)
	if err != nil {
		t.Fatal(err)
	}
	if len(found) != 2 {
		t.Fatalf("got %d namespaces, want 2: %+v", len(found), found)
	}
	// Sorted by ns.
	if found[0].Ns != "clojure-algorithms.core-test" || found[0].Stem != "core_test" {
		t.Fatalf("first: %+v", found[0])
	}
	if found[1].Ns != "clojure-algorithms.searches.linear-search-test" || found[1].Stem != "linear_search_test" {
		t.Fatalf("second: %+v", found[1])
	}
}

func TestLoadSkips(t *testing.T) {
	root := t.TempDir()
	p := filepath.Join(root, "test", "clojure_algorithms", "odd_test.clj")
	if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(p, []byte("(ns clojure-algorithms.odd-test)\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	found, err := DiscoverNamespaces(root)
	if err != nil {
		t.Fatal(err)
	}
	// Absent file: no skips.
	skips, err := LoadSkips(root, found)
	if err != nil || len(skips) != 0 {
		t.Fatalf("absent skips: %v %v", skips, err)
	}
	// Known ns: loads.
	perf := filepath.Join(root, "perf")
	if err := os.MkdirAll(perf, 0o755); err != nil {
		t.Fatal(err)
	}
	body := `{"clojure-algorithms.odd-test": "too slow"}`
	if err := os.WriteFile(filepath.Join(perf, "sweep_skips.json"), []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
	skips, err = LoadSkips(root, found)
	if err != nil || skips["clojure-algorithms.odd-test"] != "too slow" {
		t.Fatalf("load skips: %v %v", skips, err)
	}
	// Unknown ns: error.
	body = `{"clojure-algorithms.nope-test": "x"}`
	if err := os.WriteFile(filepath.Join(perf, "sweep_skips.json"), []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
	if _, err := LoadSkips(root, found); err == nil {
		t.Fatal("unknown ns must error")
	}
}

func TestDiscoverNamespacesRejectsMismatch(t *testing.T) {
	root := t.TempDir()
	p := filepath.Join(root, "test", "clojure_algorithms", "odd_test.clj")
	if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(p, []byte("(ns something.else)\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if _, err := DiscoverNamespaces(root); err == nil {
		t.Fatal("ns/path mismatch must error")
	}
}
