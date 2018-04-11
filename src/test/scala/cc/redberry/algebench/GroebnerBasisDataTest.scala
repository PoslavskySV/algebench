package cc.redberry.algebench

import java.io.{FileOutputStream, ObjectOutputStream}
import java.util.zip.GZIPOutputStream

import org.junit.Test

/**
  *
  */
class GroebnerBasisDataTest {
  @Test
  def test1(): Unit = {
    println(GroebnerBasisData.retrieveAllSystems().map(_.name).sorted.mkString("\n"))
  }

//  @Test
//  def test2(): Unit = {
//    val datas: Array[GroebnerBasisData] = GroebnerBasisData.retrieveAllSystems().toArray
//    val sss = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream("/Users/poslavskysv/Projects/redberry2/algebench/src/main/resources/gb.data.gz")))
//    sss.writeObject(datas)
//    sss.close()
//  }
//
//  @Test
//  def test3(): Unit = {
//    println(GroebnerBasisData.allSystems())
//  }
}
