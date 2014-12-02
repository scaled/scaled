//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import java.util.Arrays;
import java.util.NoSuchElementException;
import java.util.function.Predicate;
import org.junit.*;
import static org.junit.Assert.*;

public class JavaListTest {

  @Test public void testHashCode () {
    // hash code should be equivalent to Arrays.hashCode
    List<Integer> ints = Std.list(1, 2, 3, 4, 5);
    assertEquals(ints.hashCode(), Arrays.hashCode(new Integer[] { 1, 2, 3, 4, 5 }));
  }

  @Test public void testEquals () {
    List<String> c1 = Std.list("one", "two", "three");
    List<String> c2 = Std.list("one", "two", "three");
    List<String> c3 = Std.list("one", "two", "three", "four");
    List<Integer> c4 = Std.list(1, 2, 3);
    assertEquals(c1, c2);
    assertEquals(c2, c1);
    assertNotEquals(c1, c3);
    assertNotEquals(c3, c1);
    assertNotEquals(c1, c4);
    assertNotEquals(c4, c1);
  }

  @Test public void testCount () {
    List<String> c1 = Std.list("one", "two", "three");
    assertEquals(2, Lists.count(c1, s -> s.contains("o")));
    assertEquals(0, Lists.count(List.<String>nil(), s -> s.contains("o")));
    Predicate<Object> notNull = (o -> o != null);
    assertEquals(3, Lists.count(c1, notNull));
  }

  @Test public void testBuilder () {
    List.Builder<String> lb = List.builder();
    lb.append("one").append("two").append("three");
    assertEquals(List.nil().cons("three").cons("two").cons("one"), lb.build());
  }

  @Test public void testSize () {
    assertEquals(0, List.nil().size());
    List<Integer> ints = Std.list(1, 2, 3, 4, 5);
    assertEquals(5, ints.size());
    assertEquals(4, ints.tail().size());
    List<Integer> more = ints.cons(6);
    assertEquals(6, more.size());
  }

  @Test public void testConstravariance () {
    List<Integer> ints = Std.cons(1, List.nil());
    List<Number> nums = Std.cons(3f, ints);
    assertEquals(1, ints.size());
    assertEquals(2, nums.size());
  }

  @Test public void testDrop () {
    assertEquals(Std.list(3, 4, 5), Std.list(1, 2, 3, 4, 5).drop(2));
    assertEquals(List.nil(), Std.list(1, 2).drop(3));
    assertEquals(Std.list(1, 2, 3), Std.list(1, 2, 3).drop(0));
    assertEquals(Std.list(1, 2, 3), Std.list(1, 2, 3).drop(-4));

    assertEquals(Std.list(1, 2, 3), Std.list(1, 2, 3, 4, 5).dropRight(2));
    assertEquals(List.nil(), Std.list(1, 2).dropRight(3));
    assertEquals(Std.list(1, 2, 3), Std.list(1, 2, 3).dropRight(0));
    assertEquals(Std.list(1, 2, 3), Std.list(1, 2, 3).dropRight(-4));

    assertEquals(Std.list(3, 4, 5), Lists.dropWhile(Std.list(1, 2, 3, 4, 5), a -> a < 3));
    assertEquals(List.nil(), Lists.dropWhile(Std.list(1, 2), a -> a < 3));
  }

  @Test public void testEndsWith () {
    List<Integer> c1 = Std.list(1, 2, 3, 4, 5);
    List<Integer> c2 = Std.list(3, 4, 5);
    List<Integer> c3 = Std.list(0, 1, 2, 3, 4, 5);
    List<Integer> c4 = Std.list(5, 4, 3);
    assertTrue(c1.endsWith(c1));
    assertTrue(c1.endsWith(c2));
    assertTrue(c1.endsWith(List.nil()));
    assertFalse(c1.endsWith(c3));
    assertFalse(c1.endsWith(c4));
  }

  @Test public void testExists () {
    List<Integer> c1 = Std.list(1, 2, 3, 4, 5);
    assertTrue(Lists.exists(c1, a -> a == 3));
    assertFalse(Lists.exists(c1, a -> a == 9));
  }

  @Test public void testFilter () {
    List<Integer> c1 = Std.list(1, 2, 3, 4, 5);
    assertEquals(Std.list(2, 4), Lists.filter(c1, a -> a%2 == 0));
    assertEquals(Std.list(1, 3, 5), Lists.filterNot(c1, a -> a%2 == 0));
  }

  @Test public void testLast () {
    assertEquals((Integer)5, Std.list(1, 2, 3, 4, 5).last());
    assertEquals((Integer)2, Std.list(1, 2).last());
  }

  @Test(expected=NoSuchElementException.class) public void testLastOnNil () {
    assertEquals(5, List.nil().last());
  }

  @Test public void testMap () {
    assertEquals(Std.list(3, 3, 5, 4),
                 Lists.map(Std.list("one", "two", "three", "four"), s -> s.length()));
  }

  @Test public void testMinMax () {
    List<Integer> is = Std.list(9, 12, 5, 27, 3);
    assertEquals((Integer)27, is.max(Integer::compareTo));
    assertEquals((Integer) 3, is.min(Integer::compareTo));
    List<String> ss = Std.list("a", "z", "b");
    assertEquals("z", ss.max(String::compareTo));
    assertEquals("a", ss.min(String::compareTo));
  }

  @Test public void testCovariance () {
    // javac infers List<Number> return type from Std.list; yay!
    assertEquals(5, length(Std.list(1, 2, 3, 4, 5)));
    // to achieve "covariance" for an already typed list, we have to manually upcast; this is only
    // when passing to methods which lack use-site variance, but that's all of the methods in our
    // Scala API, so there you have it
    List<Integer> is = Std.list(1, 2, 3, 4, 5);
    assertEquals(5, length(is.upcast()));
  }

  private int length (List<Number> os) { return os.length(); }
}
