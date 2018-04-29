package cmd

import scala.{Option => Maybe}

sealed trait Definition

case class Option(long: String,
                  short: Maybe[Char] = None,
                  parameter: Maybe[Parameter] = None)
    extends Definition {
  def argumentAllowed: Boolean = parameter.isDefined
  def argumentRequired: Boolean = parameter.map(_.required).getOrElse(false)
  override def toString = {
    val shortString = short.map(c => s"-$c|").getOrElse("")
    val argString = parameter match {
      case None                            => ""
      case Some(Parameter(argName, false)) => s"[=<$argName>]"
      case Some(Parameter(argName, true))  => s"=<$argName>"
    }
    s"[$shortString--$long$argString]"
  }
}

case class Parameter(
    name: String,
    required: Boolean = true
) extends Definition {
  override def toString = if (required) s"<$name>" else s"[<$name>]"
}

case class Command(
    name: String,
    options: Set[Option] = Set.empty,
    parameters: Seq[Parameter] = Seq.empty,
    commands: Set[Command] = Set.empty
) extends Definition {
  override def toString = name

  def subusage(level: Int): String = {
    val optionStrings = options.map { opt =>
      opt.toString
    }
    val parameterStrings = parameters map { param =>
      param.toString
    }
    val commandStrings = Seq(commands.map(cmd => cmd.name).mkString("|"))

    val headline =
      (Seq(name) ++ optionStrings ++ parameterStrings ++ commandStrings)
        .mkString(" ")
    val sublines = commands
      .map(_.subusage(level + 1))
      .map(line => "    " * (level + 1) + line)
    headline + sublines.mkString("\n", "", "")
  }

  def usage: String = "Usage: " + subusage(0)

  def completion: String = cmd.completion.Bash.completion(this)

}
object Command {

  def apply(name: String, defs: Definition*): Command = {
    Command(
      name,
      options = defs.collect { case opt: Option         => opt }.toSet,
      parameters = defs.collect { case param: Parameter => param }.toSeq,
      commands = defs.collect { case cmd: Command       => cmd }.toSet
    )
  }

}
