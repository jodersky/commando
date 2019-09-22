[![Build Status](https://github.com/jodersky/commando/workflows/CI/badge.svg)](https://github.com/jodersky/commando/actions)

# Commando

An opinionated command line parsing utility for Scala.

```scala
libraryDependencies += "io.crashbox" %% "commando" % "<latest_version>"
```

Commando is available for Scala, Scala JS and Scala Native.

## Concepts
Commando's API is designed around two main concepts: commands and
parameters. It is recommended to read this section before diving into
the Scala API.

### Commands
Commands represent an action and have side effects when
run. Well-known examples include `ls`, `cp`, `git`, etc.

On a single command line, only one command is ever invoked. However,
commands can be nested and share *parameters*. `git` for example, fits
into this model:

```
git -p clone https://github.com/jodersky/commando
```

In the above, the top-level command has a parameter `-p`, followed by
a "subcommand" `clone`, which itself has a url as parameter. Typically,
a parent command will have several child commands. As such, a command
can also be thought of representing a on-of-many choice parameter.

### Parameters
Parameters define what arguments a command may take. There are two
kinds of parameters: positional and optional.

#### Positional parameters
Positional parameters simply get substituted by values from left to
right.

E.g. assume a command `command` that expects two positional parameters
`x` followed by `y`. Invoking the command as follows: `command 1 2`
will substitute 1 for `x` and 2 for `y`.

#### Optional parameters
Optional parameters define arguments that may be passed to a command
in any order. They typically represent "flags" or extra key-value
information.

In order to avoid ambiguity with positional arguments, optionals must
follow certain requiremenets:

- Optionals start with `--` and may be positioned anywhere within the
  scope of a command (scope of a command are all words up to the next
  subcommand).
  
  E.g. in `command --foo <arg1> --bar subcommand --baz <arg2>` the
  optionals `--foo` and `--bar` refer to `command`, and `--baz` refers
  to `subcommand`.

- May have a single-character "short" alias, in which case they an be
  specified with a single dash. E.g. `--force` and `-f`.

- Are **always** optional (use a command in case you need a
  one-of-many choice).
  
- May be repeated. `command -v -v -v -v`

- May have embedded arguments after an equals sign. `command
  --publish=80:80`

- If specified, may allow or require an argument. `command --publish
  80:80 --publish 443:443`
  
- In short form, and if they do not have parameters, arguments may be
  collapsed into a single string starting with a single
  dash. E.g. `command -i -t -f` is equivalent to `command -itf`.

- A standalone double dash is an escape sequence. Any arguments
  following are treated as positionals. E.g. `ls --
  --a-directory-starting-with---`.
  
## Scala API
The API is focused around a recursive
[`Command`](src/main/scala/definitions.scala) object. This object
represents the "grammar" of a command line application, grouping
parameter definitions and subcommands.

It is passed to a command line *parser*, along with a sequence of
*arguments* and is typically called from an application's entry point.

Commands may be constructed directly, however it is recommended to use
the domain specific language that is provided in the [commando package
object](src/main/scala/package.scala).

For example, the following defines a subset of a docker-like command
line utility:

```scala
import commando._

object Main {

  val command = cmd("docker")(
    opt("debug", 'D')
  ).sub(
    cmd("run")(
      opt("interactive", 'i'),
      opt("tty", 't'),
      pos("container")
    ).run{ arguments =>
      // run container with arguments
      println("running container " + arguments("container").head)
    },
    cmd("ps")(
      opt("all", 'a')
    ).run{ arguments =>
      if (arguments.contains("all")) {
        // list all images
      } else {
        // list running containers
      }
    }
  )

  def main(args: Array[String]): Unit = commando.parse(args, command)

}
```

Assuming the application is packaged and executable as `docker`, it
may be invoked as follows:
```
$ docker run -it my_container
running container my_container
```
```
$ docker -D ps --all
```
