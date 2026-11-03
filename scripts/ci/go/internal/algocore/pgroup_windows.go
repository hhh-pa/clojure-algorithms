//go:build windows

package algocore

import (
	"os/exec"
)

// setProcessGroup is a no-op on Windows (no process groups);
// timeouts there kill only the direct child.
func setProcessGroup(cmd *exec.Cmd) {}

// KillProcessGroup is a no-op on Windows.
func KillProcessGroup(pid int) {}
