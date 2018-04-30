package test

import commando._

object Main {

  val command = cmd("docker")(
    opt("debug", 'D')
  ).sub(
    cmd("run")(
      opt("interactive", 'i'),
      opt("tty", 't'),
      pos("container")
    ).run { arguments =>
      // run container with arguments
    },
    cmd("ps")(
      opt("all", 'a')
    ).run { arguments =>
      if (arguments.contains("all")) {
        // ...
      } else {
        // ...
      }
    }
  )

  def main(args: Array[String]): Unit = commando.parse(args, command)

}
