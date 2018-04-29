package cmd
import scala.collection.mutable
import scala.{Option => Maybe}

case class CommandLine(
    command: String,
    arguments: Map[String, String],
    subcommand: Maybe[CommandLine]
)

class ParseException(message: String) extends RuntimeException(message)

object Parser {
  private sealed trait TokenKind
  private case object SHORT extends TokenKind
  private case object LONG extends TokenKind
  private case object POSITIONAL extends TokenKind
  private case object DOUBLE_DASH extends TokenKind
  private case object EOL extends TokenKind

  private case class Token(value: String, kind: TokenKind)

  private def lex(input: Seq[String]) = new Iterator[Token] {
    val args = input.iterator
    val shortOptions = new mutable.Queue[Token]

    var escaping = false
    def hasNext = args.hasNext || !shortOptions.isEmpty

    def next(): Token =
      if (!shortOptions.isEmpty) {
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

  def parse(command: Command,
            args: Seq[String]): Either[ParseException, CommandLine] =
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

      def line(command: Command): CommandLine = {
        val longs: Map[String, Option] = command.options.map {
          case opt => opt.long -> opt
        }.toMap
        val shorts: Map[String, Option] = command.options.collect {
          case opt if opt.short.isDefined => opt.short.get.toString -> opt
        }.toMap
        val subcommands: Map[String, Command] = command.commands.map {
          case cmd => cmd.name -> cmd
        }.toMap

        def fatal(message: String) =
          throw new ParseException(s"${command.name}: $message")

        def option(): (String, String) = {
          val tok = accept()
          val parts = tok.value.split("=", 2)
          val name = parts(0)
          val embedded: Maybe[String] =
            if (parts.size > 1) Some(parts(1)) else None
          val opt = (tok.kind: @unchecked) match {
            case LONG =>
              longs.getOrElse(name, fatal(s"option --$name unknown"))
            case SHORT =>
              shorts.getOrElse(name, fatal(s"option -$name unknown"))
          }

          if (opt.argumentRequired) {
            embedded match {
              case Some(value) => opt.long -> value
              case None if token.kind == POSITIONAL =>
                opt.long -> accept().value
              case None =>
                fatal(
                  s"option ${opt} requires an argument but ${token.value} found")
            }
          } else if (opt.argumentAllowed) {
            embedded match {
              case Some(value) => opt.long -> value
              case None =>
                if (token.kind == POSITIONAL) {
                  opt.long -> accept.value
                } else {
                  opt.long -> ""
                }
            }
          } else { // no argument allowed
            embedded match {
              case Some(value) =>
                fatal(
                  s"no argument allowed for option $opt (it is set to $value)")
              case None => opt.long -> ""
            }
          }
        }

        val remainingParameters = command.parameters.iterator
        def parameter(): (String, String) = {
          if (remainingParameters.hasNext) {
            remainingParameters.next.name -> accept().value
          } else {
            fatal(s"too many parameters: '${token.value}'")
          }
        }

        val parsedOptions = new mutable.HashMap[String, String]
        val parsedParameters = new mutable.HashMap[String, String]

        var escaping = false

        def check(subline: Maybe[CommandLine]): CommandLine = {
          val remaining = remainingParameters.toList
          if (!remaining.forall(_.required == false)) {
            val missing = remaining.toList.map(p => s"'${p.name}'")
            fatal(s"missing parameter(s) ${missing.mkString(", ")}")
          } else if (!subcommands.isEmpty && subline.isEmpty) {
            val missing = command.commands.map(c => s"'${c.name}'")
            fatal(
              s"subcommand not specified (must be either one of ${missing.mkString(", ")})")
          } else {
            CommandLine(command.name,
                        parsedOptions.toMap ++ parsedParameters.toMap,
                        subline)
          }
        }

        @annotation.tailrec
        def innerLine(): CommandLine = {
          if (token.kind == EOL) {
            check(None)
          } else if (escaping) {
            parsedParameters += parameter()
            innerLine()
          } else if (token.kind == DOUBLE_DASH) {
            escaping = true
            readToken()
            innerLine()
          } else if (token.kind == POSITIONAL && !remainingParameters.isEmpty) {
            parsedParameters += parameter()
            innerLine()
          } else if (token.kind == POSITIONAL) {
            if (subcommands.isEmpty) {
              fatal(s"too many parameters: '${token.value}'")
            } else {
              subcommands.get(token.value) match {
                case None =>
                  val cmds = command.commands.map(c => s"'${c.name}'")
                  fatal(
                    s"subcommand '${token.value}' not found (must be one of ${cmds
                      .mkString(", ")})")
                case Some(_) =>
                  val subline = line(subcommands(accept().value))
                  check(Some(subline))
              }
            }
          } else if (token.kind == LONG || token.kind == SHORT) {
            parsedOptions += option()
            innerLine()
          } else {
            fatal(s"unknown token $token")
          }
        }
        innerLine()
      }
      readToken()
      Right(line(command))
    } catch {
      case ex: ParseException => Left(ex)
    }

}
