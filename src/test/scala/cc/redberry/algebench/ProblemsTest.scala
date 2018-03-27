package cc.redberry.algebench

import cc.redberry.algebench.Generators.PolynomialsDistribution
import cc.redberry.algebench.Problems.PolynomialGCDConfiguration
import cc.redberry.rings.scaladsl._
import org.junit.Test

import scala.util.Random

/**
  *
  */
class ProblemsTest {
  @Test
  def test1(): Unit = {
    import cc.redberry.algebench.Generators._
    import cc.redberry.algebench.Problems._
    import io.circe.generic.auto._
    import io.circe.syntax._
    import io.circe.parser.decode


    implicit val cfRing: Ring[IntZ] = Z
    implicit val random: Random = new Random()
    implicit val cfDistribution = CoefficientsDistribution.uniform(300)

    implicit val exponentssDistribution = ExponentsDistribution.uniform(30, 50)

    val source: PolynomialsDistribution = PolynomialsDistribution.uniform(3).apply(30)

    val conf : ProblemConfiguration = PolynomialGCDConfiguration(Z(0), Array("x"), 10,
      source, source, source)

    println(conf.asJson.noSpaces)
  }
}