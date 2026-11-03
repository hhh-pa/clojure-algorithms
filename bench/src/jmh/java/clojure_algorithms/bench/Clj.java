package clojure_algorithms.bench;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

/** Small helpers to load Clojure namespaces and resolve their functions from JMH benchmarks. */
final class Clj {
  private static final IFn REQUIRE = Clojure.var("clojure.core", "require");
  private static final String PREFIX = "clojure-algorithms.";

  private Clj() {}

  /**
   * Requires {@code clojure-algorithms.<ns>} and returns the function {@code name} defined in it.
   */
  static IFn fn(String ns, String name) {
    String fullNs = PREFIX + ns;
    REQUIRE.invoke(Clojure.read(fullNs));
    return Clojure.var(fullNs, name);
  }

  /** Returns a function from the benchmark fixtures namespace. */
  static IFn fixture(String name) {
    return fn("bench.fixtures", name);
  }
}
