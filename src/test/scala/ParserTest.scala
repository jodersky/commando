package commando

import utest._

object ParserTest extends TestSuite {

  implicit class EliteCommando(line: String) {
    def parse(command: Command): Unit = {
      val args = line.split(" ")
      commando.parse(args, command)(err => throw new ParseException(err))
    }
  }

  val tests = Tests {
    "foo" - {
      val command = cmd("cbx")(
        opt("server", 'S',  param = "url" -> false),
        pos("number")
      ).run(
        ctx => println("yoyo, my context was: " + ctx)
      )
      "--server x 3 -S 5 --server=2 --server 2".parse(command)
      println(command.usage)
    }
  }

}
