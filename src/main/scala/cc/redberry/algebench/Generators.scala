package cc.redberry.algebench


import java.util.concurrent.atomic.AtomicLong

import cc.redberry.rings.Rings
import cc.redberry.rings.poly.multivar
import cc.redberry.rings.poly.multivar.MonomialOrder
import cc.redberry.rings.scaladsl.{IntZ, MultivariatePolynomial, Ring, _}
import cc.redberry.rings.util.ArraysUtil
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

import scala.util.Random

/**
  *
  */
object Generators {

  import io.circe.generic.auto._
  import io.circe.syntax._

  /** Next random integer */
  private def randomCoefficient(characteristic: IntZ, numBits: Int)(implicit random: Random): IntZ =
    if (!characteristic.isZero)
      new IntZ(characteristic.bitLength(), random).mod(characteristic)
    else {
      if (numBits < 0)
        throw new IllegalArgumentException("cfRing must be a field")
      new IntZ(numBits, random)
    }

  /** Next random integer */
  private def nextInt(from: Int, to: Int)(implicit random: Random): Int =
    if (from == to)
      from // for simplicity
    else
      from + random.nextInt(to - from)


  sealed trait IntDistribution {
    def sample(implicit random: Random): Int
  }


  object IntDistribution {
    def uniform(min: Int, max: Int) = UniformIntDistribution(min, max)

    def fixed(value: Int) = UniformIntDistribution(value, value)
  }

  case class UniformIntDistribution(min: Int, max: Int) extends IntDistribution {
    override def sample(implicit random: Random): Int = nextInt(min, max)
  }

  /** Generates random coefficients */
  sealed trait CoefficientsDistribution {
    def sample(implicit random: Random): IntZ
  }

  object CoefficientsDistribution {
    def uniform(numBits: Int = -1)(implicit cfRing: Ring[IntZ], random: Random): CoefficientsDistribution =
      UniformCoefficientsDistribution(cfRing.characteristic(), numBits)
  }

  /** uniform distribution of integers with specified bit count */
  case class UniformCoefficientsDistribution(characteristic: IntZ, numBits: Int) extends CoefficientsDistribution {
    override def sample(implicit random: Random): IntZ = randomCoefficient(characteristic, numBits)
  }

  /** Generates random degree vectors */
  sealed trait ExponentsDistribution {
    /** next degree vector of given size */
    def sample(nVariables: Int, random: Random): Array[Int]
  }

  object ExponentsDistribution {
    def uniform(degMin: Int, degMax: Int): ExponentsDistribution =
      UniformExponentsDistribution(degMin, degMax)

    def sharp(totalDegree: Int): ExponentsDistribution =
      SharpExponentsDistribution(totalDegree)
  }

  /** Uniform distribution of exponents in degree vector */
  case class UniformExponentsDistribution(degMin: Int, degMax: Int) extends ExponentsDistribution {
    /** next degree vector of given size */
    override def sample(nVariables: Int, random: Random): Array[Int] =
      (0 until nVariables).map(_ => nextInt(degMin, degMax)(random)).toArray
  }

  /** Sharp distribution of exponents in degree vector */
  case class SharpExponentsDistribution(totalDegree: Int) extends ExponentsDistribution {
    /** next degree vector of given size */
    override def sample(nVariables: Int, random: Random): Array[Int] = {
      var sumDegree = totalDegree
      val exponents = new Array[Int](nVariables)

      val range: Seq[Int] = 0 until nVariables
      val variables = random.shuffle(range).toArray
      var i = 0
      do {
        val deg = random.nextInt(sumDegree)
        exponents(variables(i)) = deg
        sumDegree -= deg
        i += 1
      } while (sumDegree > 0)

      exponents
    }
  }

  /** Generates random polynomials */
  sealed trait PolynomialsDistribution {
    /** generate next polynomial at random */
    def sample(implicit random: Random): MultivariatePolynomial[IntZ]
  }

  object PolynomialsDistribution {
    /**
      * Factory for creating polynomial distributions with given size of polynomials
      */
    def uniform(nVariables: Int)
               (implicit cfRing: Ring[IntZ], random: Random, coefficientsDistribution: CoefficientsDistribution, expsDistribution: ExponentsDistribution)
    : Int => PolynomialsDistribution =
      nTerms => UniformPolynomialsDistribution(cfRing.characteristic(), nVariables, IntDistribution.fixed(nTerms), expsDistribution, coefficientsDistribution)

    /**
      * Polynomials distribution with uniformly distributed polynomials from minSize to maxSize
      */
    def uniform(nVariables: Int, minSize: Int, maxSize: Int)
               (implicit cfRing: Ring[IntZ], random: Random, coefficientsDistribution: CoefficientsDistribution, expsDistribution: ExponentsDistribution)
    : PolynomialsDistribution = UniformPolynomialsDistribution(cfRing.characteristic(), nVariables, IntDistribution.uniform(minSize, maxSize), expsDistribution, coefficientsDistribution)
  }

  /**
    * Uniform polynomial distributions with given size of polynomials
    */
  case class UniformPolynomialsDistribution(characteristic: IntZ,
                                            nVariables: Int,
                                            nTermsDistribution: IntDistribution,
                                            exponentsDistribution: ExponentsDistribution,
                                            coefficientsDistribution: CoefficientsDistribution) extends PolynomialsDistribution {
    private val cfRing: Ring[IntZ] = if (characteristic.isZero) Rings.Z else Rings.Zp(characteristic)

    /** generate next polynomial at random */
    override def sample(implicit random: Random): MultivariatePolynomial[IntZ] = {
      val result = multivar.MultivariatePolynomial.zero(nVariables, cfRing, MonomialOrder.DEFAULT)
      val nTerms = nTermsDistribution.sample(random)
      for (_ <- 0 to nTerms)
        result.add(new Monomial[IntZ](
          exponentsDistribution.sample(nVariables, random),
          coefficientsDistribution.sample(random)))
      result
    }
  }

  /** applies given func to each sample() */
  case class PatchedPolynomialsDistribution(innerDistribution: PolynomialsDistribution,
                                            func: MultivariatePolynomial[IntZ] => MultivariatePolynomial[IntZ])
    extends PolynomialsDistribution {

    /** generate next polynomial at random */
    override def sample(implicit random: Random): MultivariatePolynomial[IntZ] = {
      var r: MultivariatePolynomial[IntZ] = null
      do {
        r = func(innerDistribution.sample)
      } while (r == null)
      r
    }
  }

  implicit class PolynomialDistributionOps(dist: PolynomialsDistribution) {
    /** apply func to each sample */
    final def andThen(func: MultivariatePolynomial[IntZ] => MultivariatePolynomial[IntZ]): PolynomialsDistribution =
      dist match {
        case p: PatchedPolynomialsDistribution => PatchedPolynomialsDistribution(p.innerDistribution, p.func.andThen(func))
        case _ => PatchedPolynomialsDistribution(dist, func)
      }

    /** skip zeroes */
    final def skipZeroes(): PolynomialsDistribution =
      andThen(p => if (p.isZero) null else p)

    /** make all samples canonical (primitive or monic) */
    final def canonical(): PolynomialsDistribution =
      andThen(p => p.canonical())

    /** remove monomial content */
    final def noMonomialContent(): PolynomialsDistribution =
      andThen(p => p.divideOrNull(p.monomialContent()))

    /** force constant term to be non zero */
    final def withCC(): PolynomialsDistribution =
      andThen(p => if (p.ccAsPoly().isZero) p.increment() else p)

    /** track samples statistics */
    final def attachStatistics(statistics: SamplesStatistics): PolynomialsDistribution =
      andThen(p => {statistics.addValue(p); p })
  }

  private def pretty(num: Double): String = {
    if (num < 0.01)
      f"$num%6.3e"
    else
      f"$num%1.2f"
  }

  private def pretty(stat: DescriptiveStatistics): String = {
    val mean = stat.getMean
    val dev = stat.getStandardDeviation
    var res = pretty(mean)
    if (dev != 0)
      res += s" ± ${pretty(dev)}"
    res
  }

  class SamplesStatistics extends Serializable {
    val nIterations = new AtomicLong()
    val nVariables = new DescriptiveStatistics()
    val size = new DescriptiveStatistics()
    val sparsity = new DescriptiveStatistics()
    val sparsity2 = new DescriptiveStatistics()
    val minDegree = new DescriptiveStatistics()
    val maxDegree = new DescriptiveStatistics()
    val totalDegree = new DescriptiveStatistics()
    val numBits = new DescriptiveStatistics()
    val termsTotalDegree = new DescriptiveStatistics()
    val termsMinDegree = new DescriptiveStatistics()
    val termsMaxDegree = new DescriptiveStatistics()

    def addValue(poly: MultivariatePolynomial[IntZ]): Unit = {
      nIterations.incrementAndGet()

      nVariables.addValue(poly.nVariables)
      size.addValue(poly.size())
      sparsity.addValue(poly.sparsity())
      sparsity2.addValue(poly.sparsity2())

      minDegree.addValue(ArraysUtil.min(poly.degrees()))
      maxDegree.addValue(poly.degreeMax())
      totalDegree.addValue(poly.degreeSum())

      import scala.collection.JavaConverters._
      for (term: Monomial[IntZ] <- poly.asScala) {
        numBits.addValue(term.coefficient.bitLength())
        termsMinDegree.addValue(ArraysUtil.min(term.exponents))
        termsMaxDegree.addValue(ArraysUtil.max(term.exponents))
        termsTotalDegree.addValue(term.totalDegree)
      }
    }

    override def toString =
      s"""
         | # iterations        : $nIterations
         | # variables         : ${pretty(nVariables)}
         | size                : ${pretty(size)}
         | sparsity            : ${pretty(sparsity)}
         | sparsity2           : ${pretty(sparsity2)}
         | minimal degree      : ${pretty(minDegree)}
         | maximal degree      : ${pretty(maxDegree)}
         | total degree        : ${pretty(totalDegree)}
         | bit length          : ${pretty(numBits)}
         | terms min degree    : ${pretty(termsMinDegree)}
         | terms max degree    : ${pretty(termsMaxDegree)}
         | terms total degree  : ${pretty(termsTotalDegree)}
       """.stripMargin
  }

  // circe coders

  import io.circe._, io.circe.generic.semiauto._
  implicit val distDecoder: Decoder[PolynomialsDistribution] = deriveDecoder[PolynomialsDistribution]
  implicit val distEncoder: Encoder[PolynomialsDistribution] = deriveEncoder[PolynomialsDistribution]

  implicit val encodeIntZ: Encoder[IntZ] = (a: IntZ) => Json.obj(("IntZ", Json.fromString(a.toString)))

  implicit val decodeIntZ: Decoder[IntZ] = (c: HCursor) =>
    for {str <- c.downField("IntZ").as[String]}
      yield new IntZ(str)

  implicit final val encodePatchedPolynomialsDistribution: Encoder[PatchedPolynomialsDistribution] =
    (a: PatchedPolynomialsDistribution) => Json.obj(("PolynomialsDistribution", a.innerDistribution.asJson))

  implicit val decodePatchedPolynomialsDistribution: Decoder[PatchedPolynomialsDistribution] = (c: HCursor) =>
    for {r <- c.downField("PolynomialsDistribution").as[PolynomialsDistribution]}
      yield PatchedPolynomialsDistribution(r, f => f)
}
