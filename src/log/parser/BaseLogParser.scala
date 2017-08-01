package log.parser

import java.text.SimpleDateFormat

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by jiancai.wang on 2017/1/7.
  * To be use parse log by lexer
  */
trait BaseLogParser extends JavaTokenParsers {


  val TimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:SS,sss")

  def parseLog(logInfo: String): Map[String, Any] = {
    val result: ParseResult[Map[String, Any]] = parseAll(pLogger, logInfo)
    if (result.successful) {
      result.get
    } else {
      null
    }
  }


  // 日志组合解析器
  def pLogger: Parser[Map[String, Any]] =
    repsep((pActor | pTimestamp | pLevel | pPackage | pClass | pInfo), "") ^^ {
      os => {
        var map = Map[String, Any]()
        os.foreach(o => map = map ++ o)
        map
      }
    }

  // 各系统日志名称解析器
  def pActor: Parser[Map[String, Any]] =
    "[" ~> """(\w*)\-{1}(\w*)""".r <~ "]" ^^ (s => Map("serviceName" -> s))

  // 日期解析器
  def pTimestamp: Parser[Map[String, Any]] =
    "[" ~> """(\d{2}|\d{4})(?:\-)?([0]{1}\d{1}|[1]{1}[0-2]{1})(?:\-)?([0-2]{1}\d{1}|[3]{1}[0-1]{1})(?:\s)?([0-1]{1}\d{1}|[2]{1}[0-3]{1})(?::)?([0-5]{1}\d{1})(?::)?([0-5]{1}\d{1})?(\-|\,|\.)\d{1,3}""".r <~ "]" ^^ (t => Map("timestamp" -> TimeFormat.parse(t)))

  // 日志级别解析器
  def pLevel: Parser[Map[String, Any]] =
    "[" ~> """(ALL|DEBUG|ERROR|FATAL|INFO|OFF|TRACE|WARN){1}(\s)*""".r <~ "]" ^^ (s => Map("level" -> s.trim))

  //  包名解析器
  def pPackage: Parser[Map[String, Any]] =
    "-[" ~> """(\w*)\s{1}(\w*)""".r <~ "]" ^^ (s => Map("packageName" -> s))

  // 类名解析器
  def pClass: Parser[Map[String, Any]] =
    "[" ~> """(\w*)\.{1}(\w*)""".r <~ "]" ^^ (s => Map("className" -> s))

  // 详情解析器
  def pInfo: Parser[Map[String, Any]] =
    """(\:|\-)[\s\S]{0,10240}""".r ^^ (s => Map("logInfo" -> s.substring(1, s.length)))
}
