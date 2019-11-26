//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import org.junit.Assert._
import org.junit._
import scaled._

class FuzzyMatchTest {

  @Test def testScore () :Unit = {
    val m = FuzzyMatch("path")
    println(m.score("ParticleCore:tripleplay.particle.ParticleShader"))
    println(m.score("Path:playn.core"))
    println(m.filter(Seq("ParticleCore:tripleplay.particle.ParticleShader", "Path:playn.core")))

    val m2 = FuzzyMatch("path:")
    println(m2.score("Path:pythagoras.d"))
    println(m2.score("PathTest:pythagoras.d"))
  }
}
