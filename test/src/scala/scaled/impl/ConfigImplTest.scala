//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import org.junit.Assert._
import org.junit._
import scaled._
import scaled.util.Properties

class ConfigImplTest {

  object TestConfig extends Config.Defs() {
    @Var("The default width of editor views, in characters.")
    val viewWidth = key(100)
    @Var("The default height of editor views, in characters.")
    val viewHeight = key(40)
    @Var("The number of entries retained by the kill ring.")
    val killRingSize = key(40)
  }

  @Test def testReadInit () {
    val impl = new ConfigImpl("editor", null, null, TestConfig :: Nil, None)
    val props = Seq("# Scaled editor config",
                    "", "# View width", "view-width: 15",
                    "", "# View height", "view-height: 25")
    impl.read(TestData.log, Properties.read(TestData.log, "test", props))
    assertEquals(15, impl(TestConfig.viewWidth))
    assertEquals(25, impl(TestConfig.viewHeight))
  }

  @Test def testWrite () {
    val impl = new ConfigImpl("editor", null, null, TestConfig :: Nil, None)

    val allDefaults = Seq(
      "", "## The number of entries retained by the kill ring.", "# kill-ring-size: 40",
      "", "## The default height of editor views, in characters.", "# view-height: 40",
      "", "## The default width of editor views, in characters.", "# view-width: 100")
    assertTrue(impl.toProperties containsSlice allDefaults)

    impl(TestConfig.viewWidth) = 15
    val viewWidthChanged = Seq(
      "", "## The number of entries retained by the kill ring.", "# kill-ring-size: 40",
      "", "## The default height of editor views, in characters.", "# view-height: 40",
      "", "## The default width of editor views, in characters.", "view-width: 15")
    assertTrue(impl.toProperties containsSlice viewWidthChanged)
  }
}
