package clojure_algorithms.bench;

import clojure.lang.IFn;
import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.annotations.*;

@State(Scope.Benchmark)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class HashesBenchmark {
  private IFn sha256;
  private IFn sha1;
  private IFn md5;
  private IFn crc32;
  private IFn adler32;
  private IFn djb2;

  private String text1k;
  private String text4k;

  @Setup(Level.Trial)
  public void setup() {
    sha256 = Clj.fn("hashes.sha256", "sha256");
    sha1 = Clj.fn("hashes.sha1", "sha1");
    md5 = Clj.fn("hashes.md5", "md5");
    crc32 = Clj.fn("hashes.crc32", "crc32-str");
    adler32 = Clj.fn("hashes.adler32", "adler32");
    djb2 = Clj.fn("hashes.djb2", "djb2");

    IFn prose = Clj.fixture("prose");
    text1k = (String) prose.invoke(1024);
    text4k = (String) prose.invoke(4096);
  }

  @Benchmark
  public Object sha256_1k() {
    return sha256.invoke(text1k);
  }

  @Benchmark
  public Object sha1_4k() {
    return sha1.invoke(text4k);
  }

  @Benchmark
  public Object md5_4k() {
    return md5.invoke(text4k);
  }

  @Benchmark
  public Object crc32_4k() {
    return crc32.invoke(text4k);
  }

  @Benchmark
  public Object adler32_4k() {
    return adler32.invoke(text4k);
  }

  @Benchmark
  public Object djb2_4k() {
    return djb2.invoke(text4k);
  }
}
