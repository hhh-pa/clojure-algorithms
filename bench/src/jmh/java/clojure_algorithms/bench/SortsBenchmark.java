package clojure_algorithms.bench;

import clojure.lang.IFn;
import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.annotations.*;

@State(Scope.Benchmark)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class SortsBenchmark {
  private IFn mergeSort;
  private IFn quickSort;
  private IFn heapSort;
  private IFn timSort;
  private IFn insertionSort;
  private IFn shellSort;
  private IFn countingSort;

  private Object random5k;
  private Object smallRange5k;
  private Object random2k;
  private Object random1k;

  @Setup(Level.Trial)
  public void setup() {
    mergeSort = Clj.fn("sorts.merge-sort", "merge-sort");
    quickSort = Clj.fn("sorts.quick-sort", "quick-sort");
    heapSort = Clj.fn("sorts.heap-sort", "heap-sort");
    timSort = Clj.fn("sorts.tim-sort", "tim-sort");
    insertionSort = Clj.fn("sorts.insertion-sort", "insertion-sort");
    shellSort = Clj.fn("sorts.shell-sort", "shell-sort");
    countingSort = Clj.fn("sorts.counting-sort", "counting-sort");

    IFn randomInts = Clj.fixture("random-ints");
    random5k = randomInts.invoke(5000);
    // Counting sort allocates a bucket per distinct value: keep the range small.
    smallRange5k = randomInts.invoke(5000, 10000, 42);
    random2k = randomInts.invoke(2000);
    random1k = randomInts.invoke(1000);
  }

  @Benchmark
  public Object mergeSort5k() {
    return mergeSort.invoke(random5k);
  }

  @Benchmark
  public Object quickSort5k() {
    return quickSort.invoke(random5k);
  }

  @Benchmark
  public Object heapSort5k() {
    return heapSort.invoke(random5k);
  }

  @Benchmark
  public Object shellSort5k() {
    return shellSort.invoke(random5k);
  }

  @Benchmark
  public Object countingSort5k() {
    return countingSort.invoke(smallRange5k);
  }

  @Benchmark
  public Object timSort2k() {
    return timSort.invoke(random2k);
  }

  @Benchmark
  public Object insertionSort1k() {
    return insertionSort.invoke(random1k);
  }
}
