package commando

import commando.completion.Bash

sealed trait Parameter {
  def usage: String
}

case class Optional(long: String,
                    short: Option[Char] = None,
                    argumentAllowed: Boolean = false,
                    argumentRequired: Boolean = false,
                    parameterName: String = "param")
    extends Parameter {

  def usage: String = {
    val shortString = short.map(c => s"-$c|").getOrElse("")
    val paramString = if (argumentRequired) {
      s"=<$parameterName>"
    } else if (argumentAllowed) {
      s"[=<$parameterName>]"
    } else {
      ""
    }
    s"[$shortString--$long$paramString]"
  }
}

case class Positional(name: String, required: Boolean = true)
    extends Parameter {
  def usage: String = if (required) s"<$name>" else s"[<$name>]"
}

case class Command(
    name: String,
    optionals: Set[Optional],
    positionals: Seq[Positional],
    commands: Set[Command] = Set.empty,
    action: Option[Command.Arguments => Unit] = None
) {

  private def subusage(level: Int): String = {
    val optStrings = optionals.map { opt =>
      opt.usage
    }
    val posStrings = positionals map { pos =>
      pos.usage
    }
    val cmdStrings = Seq(commands.map(cmd => cmd.name).mkString("|"))

    val headline =
      (Seq(name) ++ optStrings ++ posStrings ++ cmdStrings).mkString(" ")
    val lines = commands
      .map(_.subusage(level + 1))
      .map(line => "    " * (level + 1) + line)
    headline + lines.mkString("\n", "", "")
  }

  def usage: String = subusage(0)

  def completion: String = Bash.completion(this)
}

object Command {
  type Arguments = Map[String, Seq[String]]
}