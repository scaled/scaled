//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import org.junit.*;
import static org.junit.Assert.*;

public class JavaSeqTest {

  @Test public void testBoxedPrimitives() {
    Seq<Integer> ints = Std.seq(1, 2, 3, 4, 5);
    assertEquals(5, ints.size());
  }
}
