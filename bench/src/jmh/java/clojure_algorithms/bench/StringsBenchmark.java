package clojure_algorithms.bench;

import clojure.lang.IFn;
import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.annotations.*;

@State(Scope.Benchmark)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class StringsBenchmark {
  private IFn knuthMorrisPratt;
  private IFn zFindPattern;
  private IFn rabinKarp;
  private IFn boyerMooreHorspool;
  private IFn levenshtein;
  private IFn jaroWinkler;
  private IFn manacher;

  private String text20k;
  private String pattern;
  private String wordA;
  private String wordB;
  private String text2k;

  @Setup(Level.Trial)
  public void setup() {
    knuthMorrisPratt = Clj.fn("strings.knuth-morris-pratt", "knuth-morris-pratt");
    zFindPattern = Clj.fn("strings.z-function", "find-pattern");
    rabinKarp = Clj.fn("strings.rabin-karp", "rabin-karp");
    boyerMooreHorspool = Clj.fn("strings.boyer-moore-horspool", "boyer-moore-horspool-search");
    levenshtein = Clj.fn("strings.levenshtein-distance", "levenshtein-distance");
    jaroWinkler = Clj.fn("strings.jaro-winkler", "jaro-winkler");
    manacher = Clj.fn("strings.manacher", "palindromic-string");

    IFn randomText = Clj.fixture("random-text");
    // Small alphabet => many partial matches, which stresses the matchers.
    text20k = (String) randomText.invoke(20000, 4, 7L);
    pattern = text20k.substring(15000, 15020);
    wordA = (String) randomText.invoke(300, 26, 1L);
    wordB = (String) randomText.invoke(300, 26, 2L);
    text2k = (String) randomText.invoke(2000, 3, 9L);
  }

  @Benchmark
  public Object knuthMorrisPratt20k() {
    return knuthMorrisPratt.invoke(text20k, pattern);
  }

  @Benchmark
  public Object zFunctionFindPattern20k() {
    return zFindPattern.invoke(pattern, text20k);
  }

  @Benchmark
  public Object rabinKarp20k() {
    return rabinKarp.invoke(pattern, text20k);
  }

  @Benchmark
  public Object boyerMooreHorspool20k() {
    return boyerMooreHorspool.invoke(text20k, pattern);
  }

  @Benchmark
  public Object levenshtein300() {
    return levenshtein.invoke(wordA, wordB);
  }

  @Benchmark
  public Object jaroWinkler300() {
    return jaroWinkler.invoke(wordA, wordB);
  }

  @Benchmark
  public Object manacherLongestPalindrome2k() {
    return manacher.invoke(text2k);
  }
}
