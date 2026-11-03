package clojure_algorithms.bench;

import clojure.lang.IFn;
import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.annotations.*;

@State(Scope.Benchmark)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class SearchesBenchmark {
  private IFn binarySearch;
  private IFn bisectLeft;
  private IFn quickSelect;
  private IFn linearSearch;

  private Object sorted10k;
  private Object random10k;

  @Setup(Level.Trial)
  public void setup() {
    binarySearch = Clj.fn("searches.binary-search", "binary-search");
    bisectLeft = Clj.fn("searches.binary-search", "bisect-left");
    quickSelect = Clj.fn("searches.quick-select", "quick-select");
    linearSearch = Clj.fn("searches.linear-search", "linear-search");

    sorted10k = Clj.fixture("sorted-ints").invoke(10000);
    random10k = Clj.fixture("random-ints").invoke(10000);
  }

  @Benchmark
  public Object binarySearch10k() {
    return binarySearch.invoke(sorted10k, 13370L);
  }

  @Benchmark
  public Object bisectLeft10k() {
    return bisectLeft.invoke(sorted10k, 13371L);
  }

  @Benchmark
  public Object linearSearch10k() {
    return linearSearch.invoke(sorted10k, 19998L);
  }

  @Benchmark
  public Object quickSelectMedian10k() {
    // Fixed seed so the pivot sequence (and the work done) is identical every call.
    return quickSelect.invoke(random10k, 5000L, 1234L);
  }
}
