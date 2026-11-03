package clojure_algorithms.bench;

import clojure.lang.IFn;
import java.util.List;
import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.annotations.*;

@State(Scope.Benchmark)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class DynamicProgrammingBenchmark {
  private IFn lcs;
  private IFn editDistance;
  private IFn knapsack;
  private IFn lisLength;
  private IFn matrixChainOrder;
  private IFn maxSubarraySum;
  private IFn floydWarshall;

  private String wordA150;
  private String wordB150;
  private String wordA300;
  private String wordB300;
  private Object knapsackWeights;
  private Object knapsackValues;
  private Object random10k;
  private Object signed10k;
  private Object matrixDims;
  private Object fwGraph;

  @Setup(Level.Trial)
  public void setup() {
    lcs = Clj.fn("dynamic-programming.longest-common-subsequence", "longest-common-subsequence");
    editDistance = Clj.fn("dynamic-programming.edit-distance", "min-dist-bottom-up");
    knapsack = Clj.fn("dynamic-programming.knapsack", "knapsack");
    lisLength =
        Clj.fn(
            "dynamic-programming.longest-increasing-subsequence-o-nlogn",
            "longest-increasing-subsequence-length");
    matrixChainOrder = Clj.fn("dynamic-programming.matrix-chain-order", "matrix-chain-order");
    maxSubarraySum = Clj.fn("dynamic-programming.max-subarray-sum", "max-subarray-sum");
    floydWarshall = Clj.fn("dynamic-programming.floyd-warshall", "floyd-warshall");

    IFn randomText = Clj.fixture("random-text");
    IFn randomInts = Clj.fixture("random-ints");
    wordA150 = (String) randomText.invoke(150, 26, 1L);
    wordB150 = (String) randomText.invoke(150, 26, 2L);
    wordA300 = (String) randomText.invoke(300, 26, 1L);
    wordB300 = (String) randomText.invoke(300, 26, 2L);
    // Weights in [1, 50], values in [0, 100).
    knapsackWeights = Clj.fixture("inc-all").invoke(randomInts.invoke(100, 50, 5L));
    knapsackValues = randomInts.invoke(100, 100, 6L);
    random10k = randomInts.invoke(10000);
    signed10k = Clj.fixture("random-signed-ints").invoke(10000, 1000, 8L);
    matrixDims = Clj.fixture("inc-all").invoke(randomInts.invoke(60, 100, 10L));

    IFn makeGraph = Clj.fn("dynamic-programming.floyd-warshall", "make-graph");
    IFn addEdge = Clj.fn("dynamic-programming.floyd-warshall", "add-edge");
    Object graph = makeGraph.invoke(60);
    for (Object e : (List<?>) Clj.fixture("weighted-edges").invoke(60, 400, 15L)) {
      List<?> edge = (List<?>) e;
      graph = addEdge.invoke(graph, edge.get(0), edge.get(1), edge.get(2));
    }
    fwGraph = graph;
  }

  @Benchmark
  public Object longestCommonSubsequence150() {
    return lcs.invoke(wordA150, wordB150);
  }

  @Benchmark
  public Object editDistance300() {
    return editDistance.invoke(wordA300, wordB300);
  }

  @Benchmark
  public Object knapsack100Items() {
    return knapsack.invoke(500L, knapsackWeights, knapsackValues, 100L);
  }

  @Benchmark
  public Object longestIncreasingSubsequence10k() {
    return lisLength.invoke(random10k);
  }

  @Benchmark
  public Object matrixChainOrder60() {
    return matrixChainOrder.invoke(matrixDims);
  }

  @Benchmark
  public Object maxSubarraySum10k() {
    return maxSubarraySum.invoke(signed10k);
  }

  @Benchmark
  public Object floydWarshall60() {
    return floydWarshall.invoke(fwGraph);
  }
}
