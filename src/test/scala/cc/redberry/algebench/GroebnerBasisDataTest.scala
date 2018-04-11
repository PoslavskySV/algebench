package cc.redberry.algebench

import org.junit.{Ignore, Test}

/**
  *
  */
class GroebnerBasisDataTest {
  @Ignore
  @Test
  def test1(): Unit = {
    println(GroebnerBasisData.retrieveAllSystems().map(_.name).sorted.mkString("\n"))
  }

  //  @Test
  //  def test2(): Unit = {
  //    val datas: Array[GroebnerBasisData] = GroebnerBasisData.retrieveAllSystems().toArray
  //    val sss = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream("src/main/resources/gb.data.gz")))
  //    sss.writeObject(datas)
  //    sss.close()
  //  }
  //
  //  @Test
  //  def test3(): Unit = {
  //    println(GroebnerBasisData.allSystems())
  //  }
}
