package main

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// Namespace is one benchmark: a test file plus its ns symbol.
type Namespace struct {
	// Ns is the full ns symbol, e.g.
	// clojure-algorithms.searches.linear-search-test.
	Ns string
	// Stem is the file stem, e.g. linear_search_test.
	Stem string
	// File is the test file path, repo-root-relative.
	File string
}

// DiscoverNamespaces walks test/ for *_test.clj files and maps each
// to its ns symbol (test/-strip, '/' -> '.', '_' -> '-'), verifying
// the mapping against the file's ns decl. Results sort by ns.
func DiscoverNamespaces(root string) ([]Namespace, error) {
	testDir := filepath.Join(root, "test")
	var out []Namespace
	err := filepath.WalkDir(testDir, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(d.Name(), "_test.clj") {
			return nil
		}
		rel, err := filepath.Rel(testDir, path)
		if err != nil {
			return err
		}
		mapped := strings.TrimSuffix(rel, ".clj")
		mapped = strings.ReplaceAll(mapped, string(os.PathSeparator), ".")
		mapped = strings.ReplaceAll(mapped, "_", "-")
		decl, err := readNsDecl(path)
		if err != nil {
			return fmt.Errorf("%s: %v", rel, err)
		}
		if decl != mapped {
			return fmt.Errorf("%s: ns decl %q does not match path mapping %q", rel, decl, mapped)
		}
		out = append(out, Namespace{
			Ns:   mapped,
			Stem: strings.TrimSuffix(d.Name(), ".clj"),
			File: filepath.ToSlash(rel),
		})
		return nil
	})
	if err != nil {
		return nil, err
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Ns < out[j].Ns })
	return out, nil
}

// LoadSkips reads perf/sweep_skips.json (ns symbol -> reason),
// returning an empty map when the file is absent. Unknown
// namespaces are an error (a skip must name a real benchmark).
func LoadSkips(root string, known []Namespace) (map[string]string, error) {
	path := filepath.Join(root, "perf", "sweep_skips.json")
	raw, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return map[string]string{}, nil
		}
		return nil, err
	}
	var skips map[string]string
	if err := json.Unmarshal(raw, &skips); err != nil {
		return nil, fmt.Errorf("%s: %v", path, err)
	}
	knownSet := map[string]bool{}
	for _, n := range known {
		knownSet[n.Ns] = true
	}
	for ns := range skips {
		if !knownSet[ns] {
			return nil, fmt.Errorf("%s: unknown namespace %q", path, ns)
		}
	}
	return skips, nil
}

// readNsDecl reads the ns symbol from a file's ns decl (the first
// `(ns <sym>` form near the top of the file).
func readNsDecl(path string) (string, error) {
	raw, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	text := string(raw)
	idx := strings.Index(text, "(ns ")
	if idx < 0 {
		return "", fmt.Errorf("no (ns ...) decl found")
	}
	rest := strings.TrimSpace(text[idx+len("(ns "):])
	end := strings.IndexAny(rest, " \t\r\n)]")
	if end < 0 {
		return "", fmt.Errorf("malformed (ns ...) decl")
	}
	return rest[:end], nil
}
