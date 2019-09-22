package commando

class Command(val name: String) {
  import Command._
  import collection.mutable

  private case class Parameter(
      var named: Boolean,
      var name: String,
      var short: Option[Char],
      var argName: String,
      var acceptsArg: Boolean,
      var requiresArg: Boolean,
      var required: Boolean,
      var repeated: Boolean,
      var action: Option[String] => Unit
  )

  class NamedBuilder(param: Parameter) {
    def require() = { param.required = true; this }
    def repeat() = { param.repeated = true; this }
    def action(fct: () => Unit) = { param.action = opt => fct(); this }

    def arg(name: String) = {
      param.argName = name; param.acceptsArg = true; param.requiresArg = true;
      new NamedArgBuilder(param)
    }
    def optionalArg(name: String) = {
      param.argName = name; param.acceptsArg = true; param.requiresArg = false;
      new NamedOptArgBuilder(param)
    }
  }

  class NamedArgBuilder(param: Parameter) {
    def require() = { param.required = true; this }
    def repeat() = { param.repeated = true; this }

    def action(fct: String => Unit) = {
      param.action = opt => fct(opt.get); this
    }
  }
  class NamedOptArgBuilder(param: Parameter) {
    def require() = { param.required = true; this }
    def repeat() = { param.repeated = true; this }

    def action(fct: Option[String] => Unit) = { param.action = fct; this }
  }

  class PositionalBuilder(param: Parameter) {
    def optional() = { param.required = false; this }
    def repeat() = { param.repeated = true; param.required = false; this }

    def action(fct: String => Unit) = {
      param.action = opt => fct(opt.get); this
    }
  }

  private val params = mutable.ListBuffer.empty[Parameter]

  def named(name: String, short: Char = 0): NamedBuilder = {
    val shortName = if (short == 0) None else Some(short)
    val param =
      Parameter(true, name, shortName, "", false, false, false, false, _ => ())
    params += param
    new NamedBuilder(param)
  }

  def positional(name: String): PositionalBuilder = {
    val param =
      Parameter(false, name, None, "", false, false, true, false, _ => ())
    params += param
    new PositionalBuilder(param)
  }

  /** Raise a fatal parse error. This will call parsing to fail with
    * the given message.
    */
  def error(message: String): Nothing = throw new ParseError(message)

  /** Parse this command wrt the given arguments.
    *
    * Returns 'None' if parsing was successful, or an error message otherwise.
    */
  def parse(args: Iterable[String]): Option[String] =
    try {
      var (named, positional) = params.toList.partition(_.named)

      // keeps track of which parameters have already been set
      val seen: mutable.Set[Parameter] = mutable.Set.empty[Parameter]

      val it = args.iterator
      var arg = ""
      var done = false
      def next() = if (it.hasNext) arg = it.next() else done = true
      next()

      var escaping = false

      def process(param: Parameter, value: Option[String]) = {
        param.action(value)
      }

      def readPositional(arg: String) =
        if (positional.isEmpty) {
          error("too many arguments")
        } else {
          process(positional.head, Some(arg))
          seen += positional.head
          if (!positional.head.repeated) {
            positional = positional.tail
          }
          next()
        }

      def getNamed(
          filter: Parameter => Boolean,
          friendlyName: String
      ): Parameter = named.find(filter) match {
        case None => error(s"unknown parameter: '$friendlyName'")
        case Some(param) if (!param.repeated && seen.contains(param)) =>
          error(
            s"parameter '$friendlyName' has already been given and repetitions are not allowed"
          )
        case Some(param) =>
          seen += param
          param
      }

      def getLong(name: String): Parameter =
        getNamed(p => p.name == name, s"--$name")
      def getShort(name: Char): Parameter =
        getNamed(p => p.short == Some(name), s"-$name")

      def readNamed(param: Parameter, friendlyName: String) = {
        next()
        val nextIsArg = !done && (!arg.startsWith("-") || arg == "--")

        if (param.requiresArg && nextIsArg) {
          process(param, Some(arg))
          next()
        } else if (param.requiresArg && !nextIsArg) {
          error(s"parameter '$friendlyName' requires an argument")
        } else if (param.acceptsArg && nextIsArg) {
          process(param, Some(arg))
          next()
        } else {
          process(param, None)
        }
      }

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
                process(param, Some(embeddedValue))
                next()
              } else {
                error(s"parameter '--$name' does not accept an argument")
              }
            case Array(name) =>
              readNamed(getLong(name), s"--$name")
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
              params.foreach(p => process(p, None))
              next()
            }
          } else {
            readNamed(params.head, s"-${chars.head}")
          }
        } else {
          readPositional(arg)
        }
      }

      for (param <- params) {
        if (param.required && !seen.contains(param))
          error(s"missing parameter: '${param.name}'")
      }
      None
    } catch {
      case ParseError(message) => Some(message)
    }

  def completion(): String = {
    val completions: List[String] = params.toList.filter(_.named).flatMap {
      param =>
        if (param.requiresArg) {
          List(s"--${param.name}=")
        } else if (param.acceptsArg) {
          List(s"--${param.name}", s"--${param.name}=")
        } else {
          List(s"--${param.name}")
        }
    }

    s"""|_${name}_complete() {
        |  local cur_word param_list
        |  cur_word="$${COMP_WORDS[COMP_CWORD]}"
        |  param_list="${completions.mkString(" ")}"
        |  if [[ $${cur_word} == -* ]]; then
        |    COMPREPLY=( $$(compgen -W "$$param_list" -- $${cur_word}) )
        |  else
        |    COMPREPLY=()
        |  fi
        |  return 0
        |}
        |complete -F _${name}_complete ${name}
        |""".stripMargin
  }

}
object Command {
  case class ParseError(message: String) extends RuntimeException(message)
}
