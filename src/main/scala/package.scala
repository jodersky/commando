package cmd

object `package` {
  def parse(command: Command,
            arguments: Seq[String]): Either[ParseException, CommandLine] =
    Parser.parse(command, arguments)
  def parseOrExit(command: Command, arguments: Seq[String])(
      action: CommandLine => Any): Unit = parse(command, arguments) match {
    case Left(ex) =>
      System.err.println(ex.getMessage)
      System.exit(1)
    case Right(res) => action(res)
  }
}
