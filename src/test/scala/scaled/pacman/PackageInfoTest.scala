//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman

import java.net.URL
import java.nio.file.Paths
import org.junit._
import org.junit.Assert._

class PackageInfoTest {

  val cwd = Paths.get("")

  val scaledApi = Seq(
    "   name: scaled-api",
    " source: git:https://github.com/scaled/scaled-api.git",
    "version: 1.0",
    "descrip: The API against which Scaled extensions are written.",
    " weburl: https://github.com/scaled/scaled-api/",
    "license: New BSD",
    " srcdir: src/main/scala",
    " bindir: target/classes"
  )
  val scaledSource = Source(Git, new URL("https://github.com/scaled/scaled-api.git"))

  @Test def testValid () {
    val info = PackageInfo(cwd, scaledApi)
    assertEquals(scaledSource, info.source)
    assertEquals(Nil, info.depends)
    assertTrue(info.errors.isEmpty)
  }

  @Test def testExtraCruft () {
    val info = PackageInfo(cwd, scaledApi ++ Seq(
      "bezelnut: ruh ruh",
      " peanuts: and popcorn"
    ))
    assertEquals(scaledSource, info.source)
    assertEquals(2, info.errors.size)
  }

  @Test def testDoubleSource () {
    val info = PackageInfo(cwd, scaledApi ++ Seq(
      " source: git:https://github.com/scaled/scaled-peanut.git"
    ))
    assertEquals(scaledSource, info.source)
    assertEquals(1, info.errors.size)
    assertTrue(info.errors.head startsWith "'source'")
  }

  @Test def testDepends () {
    val info = PackageInfo(cwd, scaledApi ++ Seq(
      " depend: git:https://github.com/scaled/java-mode.git",
      " depend: mvn:com.samskivert.scaled:textmate-grammar:1.0-SNAPSHOT:jar"
    ))
    assertEquals(scaledSource, info.source)
    val javaSource = Source(Git, new URL("https://github.com/scaled/java-mode.git"))
    val tmRepoId = RepoId("com.samskivert.scaled", "textmate-grammar", "1.0-SNAPSHOT",
                          "jar", Compile)
    info.errors foreach println
    assertEquals(0, info.errors.size)
    assertEquals(List(SourceDepend(javaSource), MavenDepend(tmRepoId)), info.depends)
  }
}
