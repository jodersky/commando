package commando

class CommandBuilder(name: String, params: Seq[Parameter]) {

  private def optionals =
    params.collect {
      case opt: Optional => opt
    }.toSet
  private def positionals = params.collect {
    case pos: Positional => pos
  }

  def run(action: Command.Arguments => Unit): Command =
    Command(name, optionals, positionals, Set.empty, Some(action))

  def sub(commands: Command*): Command =
    Command(name, optionals, positionals, commands.toSet, None)

}

object `package` {

  val DefaultErrorHandler: (Command, String) => Unit =
    (command: Command, err: String) => {
      System.err.println(s"${command.name}: $err")
      System.exit(2)
    }

  def parse(arguments: Seq[String],
            command: Command,
            onError: (Command, String) => Unit = DefaultErrorHandler): Unit =
    Parser.parse(arguments, command, onError)

  def cmd(name: String)(params: Parameter*): CommandBuilder =
    new CommandBuilder(name, params)
  def opt(name: String,
          short: Char = '\u0000',
          param: (String, Boolean) = ("", false)): Optional =
    Optional(
      name,
      if (short == '\u0000') None else Some(short),
      argumentAllowed = (param != ("", false)),
      argumentRequired = (param != ("", false)) && param._2,
      parameterName = if (param._1 == "") "param" else param._1
    )

  def pos(name: String, required: Boolean = true): Positional =
    Positional(name, required)

}
