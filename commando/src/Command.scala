package commando
import scala.annotation.meta.param

object Command {
  case class ParseError(message: String) extends RuntimeException(message)
}

class Command(val name: String, val description: String = "") {
  import Command._
  import collection.mutable

  private case class NamedParameter(
      var name: String,
      var short: Option[Char],
      var argName: String,
      var acceptsArg: Boolean,
      var requiresArg: Boolean,
      var action: Option[String] => Unit,
      var info: String
  )

  private case class PositionalParameter(
      var name: String,
      var optional: Boolean,
      var repeated: Boolean,
      var action: String => Unit
  )

  private val namedParams = mutable.ListBuffer.empty[NamedParameter]
  private val posParams = mutable.ListBuffer.empty[PositionalParameter]

  class NamedBuilder(param: NamedParameter) {
    def action(fct: () => Unit) = { param.action = opt => fct(); this }
    def info(text: String) = { param.info = text; this }

    def arg(name: String) = {
      param.argName = name; param.acceptsArg = true; param.requiresArg = true;
      new NamedArgBuilder(param)
    }
    def optionalArg(name: String) = {
      param.argName = name; param.acceptsArg = true; param.requiresArg = false;
      new NamedOptArgBuilder(param)
    }
  }

  class NamedArgBuilder(param: NamedParameter) {
    def action(fct: String => Unit) = {
      param.action = opt => fct(opt.get); this
    }
    def info(text: String) = { param.info = text; this }
  }
  class NamedOptArgBuilder(param: NamedParameter) {
    def action(fct: Option[String] => Unit) = { param.action = fct; this }
    def info(text: String) = { param.info = text; this }
  }

  class PositionalBuilder(param: PositionalParameter) {
    def optional() = { param.optional = true; this }
    def repeat() = { param.repeated = true; this }

    def action(fct: String => Unit) = { param.action = fct; this }
  }

  def named(name: String, short: Char = 0): NamedBuilder = {
    val shortName = if (short == 0) None else Some(short)
    val param = NamedParameter(name, shortName, "", false, false, _ => (), "")
    namedParams += param
    new NamedBuilder(param)
  }

  def positional(name: String): PositionalBuilder = {
    val param = PositionalParameter(name, false, false, _ => ())
    posParams += param
    new PositionalBuilder(param)
  }

  /** Raise a fatal parse error. This will cause parsing to fail with
    * the given message.
    */
  def error(message: String): Nothing = throw new ParseError(message)

  private val checks = mutable.ListBuffer.empty[() => Unit]

  /** Add a post-codition check.
    *
    * This function will get run after parsing
    * is complete, but before any parameter actions are invoked.
    *
    * Use the error() function in a check to signal that it failed.
    */
  def check(checkFct: => Unit) = { checks += (() => checkFct) }

  /** Parse this command against the given arguments.
    *
    * Returns 'None' if parsing was successful, or an error message otherwise.
    */
  def parse(args: Iterable[String]): Option[String] =
    try {
      val named = namedParams.result()
      var positional = posParams.result()

      val namedQueue =
        mutable.ListBuffer.empty[(NamedParameter, Option[String])]
      val posQueue = mutable.ListBuffer.empty[(PositionalParameter, String)]

      val it = args.iterator
      var arg = ""
      var done = false
      def next() = if (it.hasNext) arg = it.next() else done = true
      next()

      var escaping = false

      def readPositional(arg: String) =
        if (positional.isEmpty) {
          error("too many arguments")
        } else {
          posQueue += positional.head -> arg
          //seen += positional.head
          if (!positional.head.repeated) {
            positional = positional.tail
          }
          next()
        }

      def getLong(name: String): NamedParameter =
        named.find(_.name == name) match {
          case None        => error(s"unknown parameter: --$name")
          case Some(param) => param
        }
      def getShort(name: Char): NamedParameter =
        named.find(_.short == Some(name)) match {
          case None        => error(s"unknown parameter: -$name")
          case Some(param) => param
        }

      def readNamed(param: NamedParameter, given: String) = {
        next()
        val nextIsArg = !done && (!arg.startsWith("-") || arg == "--")

        if (param.requiresArg && nextIsArg) {
          namedQueue += param -> Some(arg)
          next()
        } else if (param.requiresArg && !nextIsArg) {
          error(s"parameter '$given' requires an argument")
        } else if (param.acceptsArg && nextIsArg) {
          namedQueue += param -> Some(arg)
          next()
        } else {
          namedQueue += param -> None
        }
      }

      // parse arguments
      while (!done) {
        if (escaping == true) {
          readPositional(arg)
        } else if (arg == "--") {
          escaping = true
          next()
        } else if (arg.startsWith("--")) {
          arg.drop(2).split("=", 2) match {
            case Array(name, embeddedValue) =>
              val param = getLong(name)
              if (param.acceptsArg) {
                namedQueue += param -> Some(embeddedValue)
                next()
              } else {
                error(s"parameter '$arg' does not accept an argument")
              }
            case Array(name) =>
              readNamed(getLong(name), arg)
          }
        } else if (arg.startsWith("-") && arg != "-") {
          val chars = arg.drop(1)
          val params = chars.map(c => getShort(c))
          if (params.length > 1) {
            if (!params.forall(!_.acceptsArg)) {
              error(
                s"only flags are allowed when multiple short parameters are given: $chars"
              )
            } else {
              params.foreach(p => namedQueue += p -> None)
              next()
            }
          } else {
            readNamed(params.head, s"-${chars.head}")
          }
        } else {
          readPositional(arg)
        }
      }

      // there should only be optional positional parameters left at this point
      for (p <- positional) {
        if (!p.optional && !p.repeated) error(s"missing parameter: '${p.name}'")
      }
      for (check <- checks) {
        check()
      }

      // process arguments
      for ((p, v) <- namedQueue) {
        p.action(v)
      }
      for ((p, v) <- posQueue) {
        p.action(v)
      }

      None
    } catch {
      case ParseError(message) => Some(message)
    }

  def completion(): String = {
    val named = namedParams.result()

    val completions: List[String] =
      named.flatMap { param =>
        param.short match {
          case Some(s) => List(s"-$s")
          case None    => Nil
        }
      }.sorted ::: named.flatMap { param =>
        if (param.requiresArg) {
          List(s"--${param.name}=")
        } else if (param.acceptsArg) {
          List(s"--${param.name}", s"--${param.name}=")
        } else {
          List(s"--${param.name}")
        }
      }.sorted

    s"""|_${name}_complete() {
        |  local cur_word param_list
        |  cur_word="$${COMP_WORDS[COMP_CWORD]}"
        |  param_list="${completions.mkString(" ")}"
        |  if [[ $${cur_word} == -* ]]; then
        |    COMPREPLY=( $$(compgen -W "$$param_list" -- $${cur_word}) )
        |    return 0
        |  fi
        |}
        |complete -F _${name}_complete -o default ${name}
        |""".stripMargin
  }

  def usage(): String = {
    val named = namedParams.result()
    val positional = posParams.result()

    val b = new mutable.StringBuilder

    // headline
    b.append("Usage: ")
    b.append(name);
    if (!named.isEmpty) {
      b.append(" [options]")
    }
    for (pos <- positional) {
      b.append(" ")
      if (pos.optional) {
        b.append("["); b.append(pos.name); b.append("]")
      } else {
        b.append(pos.name)
      }
      if (pos.repeated) {
        b.append("...")
      }
    }

    // command description
    if (!description.isEmpty) {
      b.append("\n\n")
      b.append(description)
    }

    // parameters
    if (!named.isEmpty) {
      b.append("\n\nOptions:\n")

      named.sortBy(_.name).foreach { param =>
        val short = param.short.map(c => s"  -$c, ").getOrElse("      ")
        val long = if (param.requiresArg) {
          s"--${param.name}=${param.argName}"
        } else if (param.acceptsArg) {
          s"--${param.name}[=${param.argName}]"
        } else {
          s"--${param.name}"
        }

        b.append("\n")
        if (long.length <= 22) {
          b.append(short)
          b.append(f"$long%-22s  ")
          b.append(param.info)
        } else {
          b.append(short)
          b.append(f"$long\n")
          b.append(" " * 30)
          b.append(param.info)
        }

      }
    }
    b.result()
  }

}
