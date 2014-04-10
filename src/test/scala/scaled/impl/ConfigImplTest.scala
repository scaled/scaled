//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import org.junit.Assert._
import org.junit._
import scaled._

class ConfigImplTest {

  @Test def testReadInto () {
    val impl = new ConfigImpl("editor", EditorConfig :: Nil, None)
    val props = Seq("# Scaled editor config",
                    "", "# View width", "view-width: 15",
                    "", "# View height", "view-height: 25")
    ConfigImpl.readInto("test", props, impl)
    assertEquals(15, impl(EditorConfig.viewWidth))
    assertEquals(25, impl(EditorConfig.viewHeight))
  }

  @Test def testWrite () {
    val impl = new ConfigImpl("editor", EditorConfig :: Nil, None)

    val allDefaults = Seq(
      "# Scaled editor config vars",
      "", "# The number of entries retained by the kill ring.", "# kill-ring-size: 40",
      "", "# The default height of editor views, in characters.", "# view-height: 40",
      "", "# The default width of editor views, in characters.", "# view-width: 100")
    assertEquals(allDefaults, impl.toProperties)

    impl(EditorConfig.viewWidth) = 15
    val viewWidthChanged = Seq(
      "# Scaled editor config vars",
      "", "# The number of entries retained by the kill ring.", "# kill-ring-size: 40",
      "", "# The default height of editor views, in characters.", "# view-height: 40",
      "", "# The default width of editor views, in characters.", "view-width: 15")
    assertEquals(viewWidthChanged, impl.toProperties)
  }
}
