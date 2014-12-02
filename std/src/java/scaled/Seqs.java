//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

/**
 * Operations on {@link Seq}s.
 */
public class Seqs extends Ordereds {

  /**
   * Flattens a seq of seqs into a single seq by concatenating each element in turn.
   */
  public static <A> Seq<A> flatten (Seq<? extends Iterable<A>> seq) {
    Seq.Builder<A> sb = Seq.builder();
    for (Iterable<A> as : seq) sb.append(as);
    return sb.build();
  }
}
