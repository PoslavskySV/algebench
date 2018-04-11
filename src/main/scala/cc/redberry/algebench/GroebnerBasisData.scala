package cc.redberry.algebench

import java.io.{BufferedInputStream, ObjectInputStream, ObjectOutputStream}
import java.util.zip.GZIPInputStream

import cc.redberry.rings.poly.multivar.MonomialOrder.GREVLEX
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.{Rational, WithVariables}

import scala.collection.mutable
import scala.io.Source

/**
  * Groebner basis input
  */
@SerialVersionUID(1L)
case class GroebnerBasisData(name: String,
                             ring: MultivariateRing[IntZ],
                             basis: Seq[MultivariatePolynomial[IntZ]],
                             order: Ordering) extends Serializable {
  private def writeObject(oos: ObjectOutputStream): Unit = {
    // default serialization
    oos.writeObject(name)
    oos.writeObject(ring.variables)
    oos.writeObject(ring.coefficientDomain.characteristic())
    oos.writeObject(basis)
    oos.writeObject(Util.orderEncode(order))
  }

  private def readObject(ois: ObjectInputStream): Unit = {
    val name: String = ois.readObject().asInstanceOf[String]
    val variables: Array[String] = ois.readObject().asInstanceOf[Array[String]]
    val characteristic: IntZ = ois.readObject().asInstanceOf[IntZ]
    val basis: Seq[MultivariatePolynomial[IntZ]] = ois.readObject().asInstanceOf[Seq[MultivariatePolynomial[IntZ]]]
    val order: Ordering = Util.orderDecode(ois.readObject().asInstanceOf[String])
    val ring = MultivariateRing(if (characteristic.isZero) Z else Zp(characteristic), variables)

    val clazz = classOf[GroebnerBasisData]
    val nameF = clazz.getDeclaredField("name")
    val ringF = clazz.getDeclaredField("ring")
    val basisF = clazz.getDeclaredField("basis")
    val orderF = clazz.getDeclaredField("order")

    val fields = Seq(nameF, ringF, basisF, orderF)
    fields.foreach(_.setAccessible(true))

    nameF.set(this, name)
    ringF.set(this, ring)
    basisF.set(this, basis)
    orderF.set(this, order)

    fields.foreach(_.setAccessible(false))
  }
}

object GroebnerBasisData {
  private def parseSystem(url: String): GroebnerBasisData = {
    try {
      import scala.xml.XML

      val xml = XML.load(url)
      val ring = MultivariateRing(Z, (xml \ "vars").text.split(",").map(_.trim))
      val basis = (xml \ "basis" \ "poly").map(p => ring(p.text))

      GroebnerBasisData(
        url.split("/").last.replace(".xml", ""),
        ring, basis, GREVLEX)
    } catch {
      case _: Throwable => null
    }
  }

  /**
    * Get Katsura systems
    */
  def getKatsura(): Seq[GroebnerBasisData] = {
    import cc.redberry.rings.poly.multivar.{GroebnerBasisData => GBD}
    import cc.redberry.rings.scaladsl.syntax._

    import scala.collection.JavaConverters._

    (GBD.MIN_KATSURA to GBD.MAX_KATSURA)
      .map { i =>
        val qSys: mutable.Seq[MultivariatePolynomial[Rational[IntZ]]] = GBD.katsura(i).asScala
        implicit val qRing = MultivariateRing(Q, WithVariables.defaultVars(qSys.head.nVariables))
        val zSys = qSys.map(p => p.map(Z, c => c.numerator))
        val zRing = MultivariateRing(Z, qRing.variables)
        GroebnerBasisData(s"katsura_${zRing.nVariables()}", zRing, zSys, zRing.ordering)
      }

  }

  /**
    * Retrieve all polynomial systems from SymbolicData website
    *
    */
  def retrieveSymbolicData(): Seq[GroebnerBasisData] = {
    Source.fromURL("http://symbolicdata.org/RDFData/PolynomialSystems.ttl")
      .mkString
      .split("\n")
      .filter(_.contains("sd:relatedPolynomialSystem"))
      .map(_.replace(" ", ""))
      .map(_.replace("sd:relatedPolynomialSystem", ""))
      .map(_.replace("<", ""))
      .map(_.replace(">", ""))
      .map(_.replace(";", ""))
      .map(_.replace("/Data/", "/XMLResources/"))
      .map(_ + ".xml")
      .map(parseSystem)
      .filterNot(_ == null)
  }

  /**
    * Retrieve all polynomial systems (from internet)
    *
    */
  def retrieveAllSystems(): Seq[GroebnerBasisData] = (getKatsura() ++ retrieveSymbolicData())
    .map(s => s.copy(name = s.name.toLowerCase()))
    .map(s => (s.name, s)).toMap.values.toSeq

  /**
    * Get all available polynomial systems (read from resources)
    *
    */
  lazy val allSystems: Map[String, GroebnerBasisData] = {
    val stream = new ObjectInputStream(new GZIPInputStream(new BufferedInputStream(
      GroebnerBasisData.getClass.getClassLoader.getResourceAsStream("gb.data.gz"))))
    val res = stream
      .readObject().asInstanceOf[Array[GroebnerBasisData]]
      .toSeq.map(s => (s.name, s)).toMap
    stream.close()
    res
  }

  /**
    * Get GroebnerBasisData by name
    *
    */
  def apply(name: String): GroebnerBasisData = allSystems(name.toLowerCase())
}