package cc.redberry.algebench

import cc.redberry.rings.Rings
import cc.redberry.rings.scaladsl.{IntZ, Ring}
import org.junit.Test

import scala.util.Random

/**
  *
  */
class DistributionsTest {

  @Test
  def test1(): Unit = {
    import cc.redberry.algebench.Distributions._
    import io.circe.generic.auto._
    import io.circe.syntax._
    import io.circe.parser.decode

    implicit val cfRing: Ring[IntZ] = Rings.Z
    implicit val random: Random = new Random()
    implicit val cfDistribution = CoefficientsDistribution.uniform(300)

    implicit val exponentssDistribution = ExponentsDistribution.uniform(30, 50)

    val stat = new SamplesStatistics
    val source: PolynomialsDistribution = PolynomialsDistribution.uniform(3).apply(30)
      .skipZeroes()
      .withCC()
      .noMonomialContent()
      .canonical()
      .attachStatistics(stat)


    val json = source.asJson.spaces4
    println(decode[PolynomialsDistribution](json))
    println(json)

    for (i <- 1 to 1000)
      source.sample(random)


    println(stat)
  }

}
