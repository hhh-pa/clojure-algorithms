package clojure_algorithms.bench;

import clojure.lang.IFn;
import clojure.lang.PersistentVector;
import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.annotations.*;

@State(Scope.Benchmark)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class GraphsBenchmark {
  private IFn dijkstra;
  private IFn dijkstraGrid;
  private IFn bfs;
  private IFn kruskal;
  private IFn tarjan;
  private IFn kosaraju;

  private Object weightedDigraph;
  private Object bfsGraph;
  private Object weightedEdges;
  private Object adjacencyVector;
  private Object adjacencyMap;
  private Object grid;
  private Object gridSource;
  private Object gridTarget;

  @Setup(Level.Trial)
  public void setup() {
    dijkstra = Clj.fn("graphs.dijkstra", "dijkstra");
    dijkstraGrid = Clj.fn("graphs.dijkstra-binary-grid", "dijkstra");
    bfs = Clj.fn("graphs.breadth-first-search", "bfs");
    kruskal = Clj.fn("graphs.minimum-spanning-tree-kruskal", "kruskal");
    tarjan = Clj.fn("graphs.tarjans-scc", "tarjan");
    kosaraju = Clj.fn("graphs.scc-kosaraju", "kosaraju");

    weightedDigraph = Clj.fixture("weighted-digraph").invoke(2000, 4, 11L);
    bfsGraph = Clj.fixture("bfs-graph").invoke(5000, 4, 12L);
    weightedEdges = Clj.fixture("weighted-edges").invoke(2000, 8000, 13L);
    adjacencyVector =
        Clj.fn("graphs.tarjans-scc", "create-graph")
            .invoke(2000, Clj.fixture("random-edges").invoke(2000, 4000, 14L));
    adjacencyMap = Clj.fixture("adjacency-map").invoke(2000, 4000, 14L);
    grid = Clj.fixture("binary-grid").invoke(40, 20, 16L);
    gridSource = PersistentVector.create(0L, 0L);
    gridTarget = PersistentVector.create(39L, 39L);
  }

  @Benchmark
  public Object dijkstra2000Nodes() {
    return dijkstra.invoke(weightedDigraph, "n0", "n1999");
  }

  @Benchmark
  public Object dijkstraBinaryGrid40x40() {
    return dijkstraGrid.invoke(grid, gridSource, gridTarget, Boolean.TRUE);
  }

  @Benchmark
  public Object breadthFirstSearch5000Nodes() {
    return bfs.invoke(bfsGraph, 0L);
  }

  @Benchmark
  public Object kruskalMst2000Nodes() {
    return kruskal.invoke(2000L, weightedEdges);
  }

  @Benchmark
  public Object tarjanScc2000Nodes() {
    return tarjan.invoke(adjacencyVector);
  }

  @Benchmark
  public Object kosarajuScc2000Nodes() {
    return kosaraju.invoke(adjacencyMap, 2000L);
  }
}
