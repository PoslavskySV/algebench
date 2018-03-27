package cc.redberry.algebench

import cc.redberry.rings.scaladsl._
import org.junit.Test

/**
  *
  */
class FermatSolverTest {

  @Test
  def test1: Unit = {

    import FermatSolver._
    implicit val ring = MultivariateRing(Z, Array("x1", "x2", "x3"))
    println("1 + 2a^3".replace(" ", "").replaceAll("(\\d+)([a-zA-Z])", "$1*$2"))
    println(ring show parseFermat("x3^3 - (3x2 + 3x1)x3^2 + (3x2^2 + (6x1)x2 + 3x1^2)x3 + x2^3 + (3x1)x2^2 + (3x1^2)x2 + x1^3"))
  }


}
