package example

object Main extends App {

  val cmd = new commando.Command("xorc", "") {

    val version = named("version", 'V')
      .action(() => println("version 1"))

    named("verbose", 'v')
      .info("Set verbosity level. This option may be repeated.")
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
      .info(
        s"Print bash completion. Add  ${name} --completions to your bashrc to get completions."
      )
      .action(() => println(completion()))

    named("help", 'h')
      .info("print a help message")
      .action(() => println(usage()))

  }
  cmd.parse(args) match {
    case None =>
    case Some(error) =>
      println(error)
      sys.exit(1)
  }

}
