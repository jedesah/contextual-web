package contextual.website

import rapture.uri._
import rapture.log._
import rapture.fs._

object `package` {
  val log = rapture.log.log
  implicit val logOutput = Logger(uri"file:///var/log/contextual.log")
  implicit val logLevel = logLevels.trace.logLevelImplicit
  
  import parts._
  implicit def implicitSpec(implicit severity: Severity, date: Date, time: Time, thread: Thread): Spec =
    log"""$date $time [$severity] ${sourceFile(width = 12, Right)}:${lineNo(4)} ${thread(10)}"""

}

