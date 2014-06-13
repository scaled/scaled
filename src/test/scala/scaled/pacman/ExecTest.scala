//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman

import java.nio.file.Paths
import org.junit.Assert._
import org.junit._

class ExecTest {

  val cwd = Paths.get(System.getProperty("user.dir"))

  @Test def testOutput () {
    val out = Exec.exec(cwd, "echo", "peanut").output();
    assertEquals(1, out.size());
    assertEquals("peanut", out.get(0));
  }
}
