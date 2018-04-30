package commando
import scala.collection.mutable

class ParseException(message: String) extends RuntimeException(message)

object Parser {
  private sealed trait TokenKind
  private case object SHORT extends TokenKind // -<n>
  private case object LONG extends TokenKind // --<n>
  private case object POSITIONAL extends TokenKind // <n>
  private case object DOUBLE_DASH extends TokenKind // --
  private case object EOL extends TokenKind

  private case class Token(value: String, kind: TokenKind)

  private def lex(input: Seq[String]): Iterator[Token] = new Iterator[Token] {
    val args = input.iterator
    val shortOptions = new mutable.Queue[Token]

    var escaping = false
    override def hasNext: Boolean = args.hasNext || shortOptions.nonEmpty

    override def next(): Token =
      if (shortOptions.nonEmpty) {
        shortOptions.dequeue()
      } else {
        val arg = args.next

        if (escaping) {
          Token(arg, POSITIONAL)
        } else if (arg == "--") {
          escaping = true
          Token("--", DOUBLE_DASH)
        } else if (arg.startsWith("--")) {
          Token(arg.drop(2), LONG)
        } else if (arg.startsWith("-") && arg != "-") {
          arg.drop(1).foreach { char =>
            shortOptions.enqueue(Token(char.toString, SHORT))
          }
          next()
        } else {
          Token(arg, POSITIONAL)
        }
      }
  }

  def parse(args: Seq[String],
            command: Command,
            onError: (Command, String) => Unit): Unit =
    try {
      val tokens: Iterator[Token] = lex(args)
      var token: Token = Token("end-of-line", EOL)
      def readToken(): Unit = {
        if (tokens.hasNext) {
          token = tokens.next
        } else {
          token = Token("end-of-line", EOL)
        }
      }

      def accept(): Token = {
        val tok = token
        readToken()
        tok
      }

      val _parsedArguments = new mutable.HashMap[String, List[String]]
      def addArgument(name: String, value: String): Unit =
        _parsedArguments.get(name) match {
          case None         => _parsedArguments(name) = value :: Nil
          case Some(values) => _parsedArguments(name) = value :: values
        }
      def parsedArguments =
        _parsedArguments.map {
          case (key, values) =>
            key -> values.reverse.toSeq
        }.toMap

      def line(command: Command): Unit = {
        val longs: Map[String, Optional] = command.optionals.map {
          case opt: Optional => opt.long -> opt
        }.toMap
        val shorts: Map[String, Optional] = command.optionals.collect {
          case opt: Optional if opt.short.isDefined =>
            opt.short.get.toString -> opt
        }.toMap
        val remainingPositionals = command.positionals.collect {
          case pos: Positional => pos
        }.iterator
        val subcommands: Map[String, Command] = command.commands.map { cmd =>
          cmd.name -> cmd
        }.toMap

        def fatal(message: String) = throw new ParseException(message)

        def optional(): Unit = {
          val tok = accept()
          val parts = tok.value.split("=", 2)
          val name = parts(0)
          val embedded: Option[String] =
            if (parts.size > 1) Some(parts(1)) else None
          val opt = (tok.kind: @unchecked) match {
            case LONG =>
              longs.getOrElse(name, fatal(s"unknown option '--$name'"))
            case SHORT =>
              shorts.getOrElse(name, fatal(s"unknown option '-$name'"))
          }

          if (opt.argumentRequired) {
            embedded match {
              case Some(value) =>
                addArgument(opt.long, value)
              case None if token.kind == POSITIONAL =>
                addArgument(opt.long, accept().value)
              case None =>
                fatal(
                  s"option ${opt.usage} requires an argument but ${token.value} found")
            }
          } else if (opt.argumentAllowed) {
            embedded match {
              case Some(value) =>
                addArgument(opt.long, value)
              case None =>
                if (token.kind == POSITIONAL) {
                  addArgument(opt.long, accept().value)
                } else {
                  addArgument(opt.long, "")
                }
            }
          } else { // no argument allowed
            embedded match {
              case Some(value) =>
                fatal(
                  s"no argument allowed for option ${opt.usage} (it is set to $value)")
              case None => addArgument(opt.long, "")
            }
          }
        }

        def positional(): Unit = {
          if (remainingPositionals.hasNext) {
            addArgument(
              remainingPositionals.next.name,
              accept().value
            )
          } else {
            fatal(s"too many arguments: '${token.value}'")
          }
        }

        // make sure all required positional parameters have been parsed
        def checkPositionals(): Unit = {
          val remaining = remainingPositionals.toList
          if (!remaining.forall(_.required == false)) {
            val missing = remaining.map(p => s"'${p.name}'")
            fatal(s"missing parameter(s) ${missing.mkString(", ")}")
          }
        }

        var escaping = false
        @annotation.tailrec
        def innerLine(): Unit = {
          if (token.kind == EOL) {
            checkPositionals()
            if (subcommands.nonEmpty) {
              val missing = command.commands.map(c => s"'${c.name}'")
              fatal(
                s"command not specified (must be one of ${missing.mkString(", ")})")
            }
            command.action.get(parsedArguments)
          } else if (escaping) {
            positional()
            innerLine()
          } else if (token.kind == DOUBLE_DASH) {
            escaping = true
            readToken()
            innerLine()
          } else if (token.kind == POSITIONAL && remainingPositionals.nonEmpty) {
            positional()
            innerLine()
          } else if (token.kind == POSITIONAL) {
            if (subcommands.isEmpty) {
              fatal(s"too many arguments: '${token.value}'")
            } else {
              subcommands.get(token.value) match {
                case None =>
                  val cmds = command.commands.map(c => s"'${c.name}'")
                  fatal(
                    s"command '${token.value}' not found (must be one of ${cmds
                      .mkString(", ")})")
                case Some(cmd) =>
                  checkPositionals()
                  readToken()
                  line(cmd)
              }
            }
          } else if (token.kind == LONG || token.kind == SHORT) {
            optional()
            innerLine()
          } else {
            fatal(s"unknown token $token")
          }
        }
        innerLine()
      }
      readToken()
      line(command)
    } catch {
      case ex: ParseException => onError(command, ex.getMessage)
    }

}
