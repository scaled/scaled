//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.code

import org.junit.Assert._
import org.junit._
import scaled._

class BlockerTest {

  val testJavaCode = Seq(
    //                1         2         3         4         5         6         7         8
    //      012345678901234567890123456789012345678901234567890123456789012345678901234567890123456
    /* 0*/ "package foo;",
    /* 1*/ "",
    /* 2*/ "/** This is some test Java code that we'll use to test {@code Grammar} and specifically",
    /* 3*/ " * the {@literal JavaDoc} grammar.",
    /* 4*/ " * @see http://manual.macromates.com/en/language_grammars",
    /* 5*/ " */",
    /* 6*/ "public class Test {",
    /* 7*/ "   /**",
    /* 8*/ "    * A constructor, woo!",
    /* 8*/ "    * @param foo for fooing.",
    /*10*/ "    * @param bar for barring.",
    /*11*/ "    */",
    /*12*/ "   public Test () {}",
    /*13*/ "",
    /*14*/ "   /**",
    /*15*/ "    * A method. How exciting. Let's {@link Test} to something.",
    /*16*/ "    * @throws IllegalArgumentException if we feel like it.",
    /*17*/ "    */",
    /*18*/ "   @Deprecated(\"Use peanuts\")",
    /*19*/ "   public void test (int count) {}",
    /*20*/ "}")

  @Test def testBlocker () {
    val buf = BufferTest.bufferV("Test.java", testJavaCode.map(Line.apply))
    val blocker = new Blocker(buf, "{([", "})]")
    // println(blocker)
    assertEquals(None, blocker(Loc(0, 0)))
    assertEquals(Loc(2,55), blocker(Loc(2, 62)).get.start)
    assertEquals(Loc(2,55), blocker(Loc(2, 70)).get.start)
    assertEquals(None, blocker(Loc(2, 71)))
    assertEquals(Loc(6,18), blocker(Loc(7, 3)).get.start)
    assertEquals(Loc(6,18), blocker(Loc(12, 14)).get.start)
    assertEquals(Loc(6,18), blocker(Loc(12, 15)).get.start)
    assertEquals(Loc(12,15), blocker(Loc(12, 16)).get.start)
    assertEquals(Loc(12,15), blocker(Loc(12, 17)).get.start)
    assertEquals(Loc(6,18), blocker(Loc(13, 0)).get.start)
  }
}
