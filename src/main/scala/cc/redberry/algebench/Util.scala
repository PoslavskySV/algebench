package cc.redberry.algebench

import java.io.File

/**
  *
  */
object Util {


  def createTempFile(name: String)(implicit t: TempFileManager): File = t.createTempFile(name)

  final case class TempFileManager(deleteOnExit: Boolean = true) {
    private val currentDir = new File(System.getProperty("user.dir"))

    def createTempFile(name: String, suff : String = null): File = {
      val file = File.createTempFile(name, suff, currentDir)
      if (deleteOnExit)
        file.deleteOnExit()
      file
    }
  }
}
