//go:build !windows

package algocore

import (
	"os/exec"
	"syscall"
)

// setProcessGroup makes the child a group leader so KillProcessGroup
// can reap Pin/perf/time grandchildren on timeout.
func setProcessGroup(cmd *exec.Cmd) {
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
}

// KillProcessGroup kills the whole group of a timed-out child.
// Errors (already dead) are ignored by design.
func KillProcessGroup(pid int) {
	_ = syscall.Kill(-pid, syscall.SIGKILL)
}
