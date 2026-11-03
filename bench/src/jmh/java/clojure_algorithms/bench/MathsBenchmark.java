package clojure_algorithms.bench;

import clojure.lang.BigInt;
import clojure.lang.IFn;
import java.math.BigInteger;
import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.annotations.*;

@State(Scope.Benchmark)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class MathsBenchmark {
  private IFn sieveOfEratosthenes;
  private IFn sieveOfAtkin;
  private IFn segmentedSieve;
  private IFn isPrime;
  private IFn primeFactors;
  private IFn totient;
  private IFn fibIterative;
  private IFn karatsuba;

  private Object bigA;
  private Object bigB;

  @Setup(Level.Trial)
  public void setup() {
    sieveOfEratosthenes = Clj.fn("maths.sieve-of-eratosthenes", "prime-sieve");
    sieveOfAtkin = Clj.fn("maths.sieve-of-atkin", "sieve-of-atkin");
    segmentedSieve = Clj.fn("maths.segmented-sieve", "sieve");
    isPrime = Clj.fn("maths.prime-check", "is-prime");
    primeFactors = Clj.fn("maths.prime-factors", "prime-factors");
    totient = Clj.fn("maths.eulers-totient", "totient");
    fibIterative = Clj.fn("maths.fibonacci", "fib-iterative");
    karatsuba = Clj.fn("maths.karatsuba", "karatsuba");

    bigA = BigInt.fromBigInteger(new BigInteger("31415926535897932384626433832795028841"));
    bigB = BigInt.fromBigInteger(new BigInteger("27182818284590452353602874713526624977"));
  }

  @Benchmark
  public Object sieveOfEratosthenes100k() {
    return sieveOfEratosthenes.invoke(100000L);
  }

  @Benchmark
  public Object sieveOfAtkin100k() {
    return sieveOfAtkin.invoke(100000L);
  }

  @Benchmark
  public Object segmentedSieve100k() {
    return segmentedSieve.invoke(100000L);
  }

  @Benchmark
  public Object isPrimeLargePrime() {
    return isPrime.invoke(1000000007L);
  }

  @Benchmark
  public Object primeFactorsSemiprime() {
    // 999983 * 1000003
    return primeFactors.invoke(999985999949L);
  }

  @Benchmark
  public Object eulersTotient100k() {
    return totient.invoke(100000L);
  }

  @Benchmark
  public Object fibonacciIterative1000() {
    return fibIterative.invoke(1000L);
  }

  @Benchmark
  public Object karatsuba38Digits() {
    return karatsuba.invoke(bigA, bigB);
  }
}
