// Command algoperf measures the algorithms repo's test namespaces
// under load-independent instruments (GNU time, perf, valgrind
// callgrind) for JVM-vs-native comparison:
//
//	algoperf measure  per-namespace perf sweep (see measure.go)
//
// Run from the repository root.
package main

import (
	"fmt"
	"os"
)

const usage = `Usage: algoperf <subcommand> [flags]

Subcommands:
  measure   per-namespace perf sweep
`

func main() {
	os.Exit(dispatch(os.Args[1:]))
}

func dispatch(argv []string) int {
	if len(argv) == 0 {
		fmt.Print(usage)
		return 2
	}
	switch argv[0] {
	case "-h", "--help", "help":
		fmt.Print(usage)
		return 0
	case "measure":
		return runMeasure(argv[1:])
	default:
		fmt.Fprintf(os.Stderr, "algoperf: unknown subcommand %q\n%s", argv[0], usage)
		return 2
	}
}
