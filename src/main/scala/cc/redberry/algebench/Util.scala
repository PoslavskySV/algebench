package cc.redberry.algebench

import java.io.File

import scala.concurrent.duration._

/**
  *
  */
object Util {


  def createTempFile(name: String)(implicit t: TempFileManager): File = t.createTempFile(name)

  final case class TempFileManager(deleteOnExit: Boolean = true) {
    private val currentDir = new File(System.getProperty("user.dir"))

    def createTempFile(name: String, suff: String = null): File = {
      val file = File.createTempFile(name, suff, currentDir)
      if (deleteOnExit)
        file.deleteOnExit()
      file
    }
  }

  def prettyDuration(duration: Duration, includeNanos: Boolean = false, precision: Int = 4): String = {
    require(precision > 0, "precision must be > 0")

    duration match {
      case d: FiniteDuration =>
        val nanos = d.toNanos
        val unit = chooseUnit(nanos)
        val value = nanos.toDouble / NANOSECONDS.convert(1, unit)

        s"%.${precision}g %s%s".format(value, abbreviate(unit), if (includeNanos) s" ($nanos ns)" else "")

      case d: Duration.Infinite if d == Duration.MinusInf => s" -âˆž (minus infinity)"
      case d => s"âˆž (infinity)"
    }
  }

  private def chooseUnit(nanos: Long): TimeUnit = {
    val d = nanos.nanos
    if (d.toDays > 0) DAYS
    else if (d.toHours > 0) HOURS
    else if (d.toMinutes > 0) MINUTES
    else if (d.toSeconds > 0) SECONDS
    else if (d.toMillis > 0) MILLISECONDS
    else if (d.toMicros > 0) MICROSECONDS
    else NANOSECONDS
  }

  private def abbreviate(unit: TimeUnit): String = unit match {
    case NANOSECONDS => "ns"
    case MICROSECONDS => "μs"
    case MILLISECONDS => "ms"
    case SECONDS => "s"
    case MINUTES => "min"
    case HOURS => "h"
    case DAYS => "d"
  }
}
