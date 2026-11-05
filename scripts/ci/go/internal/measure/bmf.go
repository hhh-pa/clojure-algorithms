package measure

import (
	"sort"

	"github.com/hhh-pa/clojure-algorithms/scripts/ci/go/internal/pyjson"
)

// Measure is one BMF measure (value with lower/upper bounds).
type Measure struct {
	Value      pyjson.Num `json:"value"`
	LowerValue pyjson.Num `json:"lower_value"`
	UpperValue pyjson.Num `json:"upper_value"`
}

// Benchmark is one namespace's BMF entry.
type Benchmark struct {
	CpuTime         *Measure `json:"cpu_time,omitempty"`
	PeakMemory      *Measure `json:"peak_memory,omitempty"`
	AllocatedBytes  *Measure `json:"allocated_bytes,omitempty"`
	PinInstructions *Measure `json:"pin_instructions,omitempty"`
}

// BMF maps benchmark name (the ns symbol) to measures.
type BMF map[string]Benchmark

// BuildBMF builds one Benchmark per namespace with
// cpu_time/peak_memory plus allocated_bytes and pin_instructions
// when the namespace reports them.
func BuildBMF(results []CaseResult) BMF {
	out := BMF{}
	for i := range results {
		res := &results[i]
		// Pin-only summaries (Runs 0) carry instrument
		// counts but no time/memory: BMF tracks timed
		// signals, so they stay out (and can never emit
		// zero cpu_time/peak_memory rows).
		if !res.Ok || res.Summary == nil || res.Summary.Runs == 0 {
			continue
		}
		s := res.Summary
		bench := Benchmark{
			CpuTime: &Measure{
				Value:      s.CpuS.Median,
				LowerValue: s.CpuS.Min,
				UpperValue: s.CpuS.Max,
			},
			PeakMemory: &Measure{
				Value:      s.MaxRSSKb.Median.Mul(pyjson.IntNum(1024)),
				LowerValue: s.MaxRSSKb.Min.Mul(pyjson.IntNum(1024)),
				UpperValue: s.MaxRSSKb.Max.Mul(pyjson.IntNum(1024)),
			},
		}
		if s.AllocatedBytes != nil {
			bench.AllocatedBytes = &Measure{
				Value:      pyjson.IntNum(s.AllocatedBytes.Min),
				LowerValue: pyjson.IntNum(s.AllocatedBytes.Min),
				UpperValue: pyjson.IntNum(s.AllocatedBytes.Max),
			}
		}
		// Pin noise is one-sided like allocation (cold paths
		// only add instructions), so the minimum is the point
		// estimate here too.
		if s.PinInstructions != nil {
			bench.PinInstructions = &Measure{
				Value:      s.PinInstructions.Min,
				LowerValue: s.PinInstructions.Min,
				UpperValue: s.PinInstructions.Max,
			}
		}
		out[res.Label] = bench
	}
	return out
}

// SortedNames returns BMF benchmark names in order.
func SortedNames(bmf BMF) []string {
	names := make([]string, 0, len(bmf))
	for name := range bmf {
		names = append(names, name)
	}
	sort.Strings(names)
	return names
}
