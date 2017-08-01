package log.parser

/**
  * Created by Revolt on 2017/8/1.
  */
object main extends BaseLogParser{

  def main(args: Array[String]) {
    parseLog("[2017-03-13 14:54:47,267][INFO ][init.StartJetty]-[2017-03-13 14:54:47] Console-Server is starting......")
  }
}
