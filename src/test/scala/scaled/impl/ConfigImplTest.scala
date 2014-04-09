//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import org.junit.Assert._
import org.junit._
import scaled._

class ConfigImplTest {

  @Test def testReadInto () {
    val impl = new ConfigImpl("scaled", EditorConfig :: Nil, None)
    val json = """{ "view-width": "15",
                    "view-height": "25" }"""
    ConfigImpl.readInto("test", json, impl)
    assertEquals(15, impl(EditorConfig.viewWidth))
    assertEquals(25, impl(EditorConfig.viewHeight))
  }

  @Test def testWrite () {
    val impl = new ConfigImpl("scaled", EditorConfig :: Nil, None)

    val allDefaults = """{
  "kill-ring-size.default": "40",
  "view-height.default": "40",
  "view-width.default": "100"
}"""
    assertEquals(allDefaults, impl.toJson)

    impl(EditorConfig.viewWidth) = 15
    val viewWidthChanged = """{
  "kill-ring-size.default": "40",
  "view-height.default": "40",
  "view-width": "15"
}"""
    assertEquals(viewWidthChanged, impl.toJson)
  }
}
