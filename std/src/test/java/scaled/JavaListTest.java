//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import org.junit.*;
import static org.junit.Assert.*;

public class JavaListTest {

  @Test public void testSize () {
    assertEquals(0, Data.nil().size());
    List<Integer> ints = Data.list(1, 2, 3, 4, 5);
    assertEquals(5, ints.size());
    assertEquals(4, ints.tail().size());
  }

  @Test public void testConstravariance () {
    List<Integer> ints = Data.cons(1, Data.nil());
    List<Number> nums = Data.cons(3f, ints);
    assertEquals(1, ints.size());
    assertEquals(2, nums.size());
  }
}
