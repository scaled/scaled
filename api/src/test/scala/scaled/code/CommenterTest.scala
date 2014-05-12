//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.code

import org.junit.Assert._
import org.junit._
import scaled._

class CommenterTest {
  import CodeConfig._

  val commentStyles = Styles(commentStyle)

  def toLine (styles :Styles)(tup :(String,Int)) = {
    val (tx, ci) = tup
    Line.builder(tx).withStyles(styles, ci, tx.length).build()
  }

  val lineText = Seq(("//", 0),
                     ("// this is a line comment", 0),
                     ("// with a couple of lines and a prefix", 0),
                     ("foo(); // and this one has code in front of it", 7))

  @Test def testCommentStart () {
    val buf = BufferTest.bufferV("lines", lineText map toLine(commentStyles))
    val cm = new Commenter(buf) {
      override def commentPrefix = "//"
      override def docPrefix = "*"
    }
    assertEquals(2, cm.commentStart(buf.line(0)))
    assertEquals(3, cm.commentStart(buf.line(1)))
    assertEquals(3, cm.commentStart(buf.line(2)))
    assertEquals(10, cm.commentStart(buf.line(3)))
  }

  val fillText = Seq(("// this is a line comment", 0),
                     ("// with a few lines that should be long enough", 0),
                     ("// to necessitate multiple lines for a refill", 0))

  @Test def testRefill () {
    val buf = BufferTest.bufferV("lines", fillText map toLine(commentStyles))
    val cm = new Commenter(buf) {
      override def commentPrefix = "//"
      override def docPrefix = "*"
    }
    val filled = cm.refillComments(80, Loc(0, 0), Loc(2, buf.line(2).length))
    assertEquals(2, filled.length)
    assertEquals(72, filled(0).length)
    assertEquals(42, filled(1).length)
  }

  val extraSpaces = Seq(("//  this is a line comment", 0),
                        ("//  with a few lines that should be long enough", 0),
                        ("//  to necessitate multiple lines for a refill", 0))

  @Test def testRefillMatchesWhitespace () {
    val buf = BufferTest.bufferV("lines", extraSpaces map toLine(commentStyles))
    val cm = new Commenter(buf) {
      override def commentPrefix = "//"
      override def docPrefix = "*"
    }
    val filled = cm.refillComments(80, Loc(0, 0), Loc(2, buf.line(2).length))
    assertEquals(2, filled.length)
    assertEquals(73, filled(0).length)
    assertTrue(filled(0).toString startsWith "//  ")
    assertEquals(43, filled(1).length)
    assertTrue(filled(1).toString startsWith "//  ")
  }
}
