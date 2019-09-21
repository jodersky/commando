package example

object Main extends App {

  val cmd = new commando.Command("xorc") {
    val version = named("version")
      .action(() => println("version 1"))
      .repeat()

    named("verbose")
      .optionalArg("level")
      .action(level => println(s"level $level"))

    positional("FILES")
      .action { s =>
        val f = new java.io.File(s)
        if (!f.exists()) error(s"File $s does not exit")
        println(f)
      }
      .repeat()

    val comp = named("completion")
      .action(() => println(completion()))
  }
  cmd.parse(args) match {
    case None =>
    case Some(error) =>
      println(error)
      sys.exit(1)
  }

}
