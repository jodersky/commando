package commando
package completion

object Bash {

  private def addCommands(command: Command): String = {
    command.commands.map(c => s"""commands+=("${c.name}")\n""").mkString
  }

  private def addFlags(command: Command): String = {
    command.optionals.map { opt =>
      val extra = if (opt.argumentRequired) "=" else ""
      val short = opt.short.map(c => s"""flags+=("-$c")""").getOrElse("")
      s"""|flags+=("--${opt.long}$extra")
          |$short
          |""".stripMargin
    }.mkString
  }

  private def commandBlocks(previous: String,
                            commands: Set[Command]): String = {
    def block(previous: String, command: Command): String = {
      s"""_${previous}_${command.name}() {
         |    ${addCommands(command)}
         |    ${addFlags(command)}
         |    true
         |}
         |""".stripMargin
    }

    if (commands.isEmpty) {
      ""
    } else {
      commands.map { cmd =>
        block(previous, cmd) + commandBlocks(cmd.name, cmd.commands)
      }.mkString
    }
  }

  def completion(command: Command): String = {
    val name = command.name

    s"""__${name}_contains_word() {
       |    local word="$$1"; shift
       |    for w in "$$@"; do
       |        [[ $$w = "$$word" ]] && return 0
       |    done
       |    return 1
       |}
       |
       |__${name}_handle_reply() {
       |    case "$$cur" in
       |        -*)
       |            COMPREPLY=( $$(compgen -W "$${flags[*]}" -- "$$cur") )
       |            if [[ $${#COMPREPLY[@]} -eq 1 ]] && [[ $${COMPREPLY[0]} == *= ]]; then
       |                compopt -o nospace
       |            else
       |                compopt +o nospace
       |            fi
       |            ;;
       |        *)
       |            COMPREPLY=( $$(compgen -W "$${commands[*]}" -- "$$cur") )
       |            ;;
       |    esac
       |}
       |
       |__${name}_handle_word() {
       |    if [[ $$c -ge $$cword ]]; then
       |        __${name}_handle_reply
       |        return
       |    fi
       |    if __${name}_contains_word "$${words[c]}" "$${commands[@]}"; then
       |        local next_command="$${last_command}_$${words[c]}"
       |        last_command="$$next_command"
       |        commands=()
       |        flags=()
       |        $$next_command
       |    fi
       |    c=$$((c+1))
       |    __${name}_handle_word
       |}
       |
       |${commandBlocks(name, command.commands)}
       |
       |__${name}_start() {
       |    local words=("$${COMP_WORDS[@]}")
       |    local cword="$$COMP_CWORD"
       |    local cur="$${COMP_WORDS[COMP_CWORD]}"
       |    local c=0
       |    COMPREPLY=()
       |
       |    local last_command="_$name"
       |    local commands=()
       |    local flags=()
       |
       |    ${addCommands(command)}
       |    ${addFlags(command)}
       |
       |    __${name}_handle_word
       |
       |    return 0
       |}
       |complete -o default -F __${name}_start $name
       |""".stripMargin
  }

}
