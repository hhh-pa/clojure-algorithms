package clojure_algorithms.bench;

import clojure.lang.IFn;
import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.annotations.*;

@State(Scope.Benchmark)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class DataCompressionBenchmark {
  private IFn huffmanEncode;
  private IFn lz77Compress;
  private IFn bwtTransform;
  private IFn runLengthEncode;
  private IFn lzwCompress;

  private Object lz77Compressor;
  private String prose4k;
  private String prose1k;
  private String prose500;
  private String binaryRuns4k;
  private String bits4k;

  @Setup(Level.Trial)
  public void setup() {
    huffmanEncode = Clj.fn("data-compression.huffman", "encode-text");
    lz77Compress = Clj.fn("data-compression.lz77", "compress");
    bwtTransform = Clj.fn("data-compression.burrows-wheeler", "bwt-transform");
    runLengthEncode = Clj.fn("data-compression.run-length-encoding", "run-length-encode");
    lzwCompress = Clj.fn("data-compression.lempel-ziv", "compress-data");

    lz77Compressor = Clj.fn("data-compression.lz77", "make-compressor").invoke();

    IFn prose = Clj.fixture("prose");
    IFn randomText = Clj.fixture("random-text");
    prose4k = (String) prose.invoke(4096);
    prose1k = (String) prose.invoke(1000);
    prose500 = (String) prose.invoke(500);
    binaryRuns4k = (String) randomText.invoke(4096, 2, 3L);
    // "a"/"b" -> "0"/"1" bit string for the LZW compressor.
    bits4k = binaryRuns4k.replace('a', '0').replace('b', '1');
  }

  @Benchmark
  public Object huffmanEncode4k() {
    return huffmanEncode.invoke(prose4k);
  }

  @Benchmark
  public Object lz77Compress1k() {
    return lz77Compress.invoke(lz77Compressor, prose1k);
  }

  @Benchmark
  public Object burrowsWheelerTransform500() {
    return bwtTransform.invoke(prose500);
  }

  @Benchmark
  public Object runLengthEncode4k() {
    return runLengthEncode.invoke(binaryRuns4k);
  }

  @Benchmark
  public Object lempelZivCompress4k() {
    return lzwCompress.invoke(bits4k);
  }
}
