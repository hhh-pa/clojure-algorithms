// Package algocore holds the logic shared by the algoperf
// subcommands: repository discovery, classpath resolution, output
// normalization, subprocess execution, and shard selection.
// Ported from the snippet repo's snipcore package (same behavior:
// messages, ordering, quirks); snippet-case discovery stays behind.
package algocore

import (
	"os"
)

// RepoRoot returns the repository root, which for the Go tools is the
// process working directory: algoperf runs from the repo root (CI
// checks out and runs there; local runs do the same).
func RepoRoot() string {
	wd, err := os.Getwd()
	if err != nil {
		return "."
	}
	return wd
}
