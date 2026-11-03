package algocore

import (
	"strings"
)

// LastLine takes the last non-blank stderr line for subprocess
// failure diagnostics ("no stderr" when empty).
func LastLine(stderr []byte) string {
	text := strings.TrimSpace(string(stderr))
	if text == "" {
		return "no stderr"
	}
	lines := strings.Split(text, "\n")
	return strings.TrimSpace(lines[len(lines)-1])
}

// TruncateChars cuts text to max runes.
func TruncateChars(text string, max int) string {
	runes := []rune(text)
	if len(runes) > max {
		return string(runes[:max])
	}
	return text
}
