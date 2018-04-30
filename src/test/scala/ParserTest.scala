package commando

import utest._

object ParserTest extends TestSuite {

  implicit class EliteCommando(line: String) {
    def parse(command: Command): Unit = {
      val args = line.split(" ").tail
      commando.parse(args, command, (c, err) => throw new ParseException(err))
    }
    def fail(command: Command, message: String): Unit = {
      val args = line.split(" ").tail
      try {
        commando.parse(args, command, (c, err) => throw new ParseException(err))
        sys.error("parsing succeeded but was expected to fail")
      } catch {
        case err: ParseException if err.getMessage.contains(message) =>
        case err: ParseException =>
          sys.error(s"parsing failed for the wrong reason: ${err.getMessage}")
      }
    }
  }

  def cbx(asserts: Command.Arguments => Unit) =
    cmd("cbx")(
      opt("server", 's', "name" -> true)
    ).sub(
      cmd("version")(
        opt("verbose", 'v', "k=v" -> false)
      ).run(asserts),
      cmd("login")(
        pos("server_url"),
        pos("username", false),
        pos("password", false)
      ).run(asserts),
      cmd("run")(
        opt("file", 'f', "file_name" -> true),
        opt("force"),
        pos("pipeline", false)
      ).run(asserts),
      cmd("level1")().sub(
        cmd("level2-1")(
          pos("p2")
        ).sub(
          cmd("level3")(pos("p3")).run(asserts)
        ),
        cmd("level2-2")().run(asserts)
      )
    )

  val tests = Tests {
    "print usage" - {
      cbx(_ => ()).usage
    }
    "simple" - {
      "cbx version" parse cbx { args =>
        args ==> Map.empty
      }
    }
    "empty allowed optional" - {
      "cbx version -v" parse cbx { args =>
        args ==> Map("verbose" -> Seq(""))
      }
      "cbx version --verbose" parse cbx { args =>
        args ==> Map("verbose" -> Seq(""))
      }
    }
    "set allowed optional" - {
      "cbx version -v x" parse cbx { args =>
        args ==> Map("verbose" -> Seq("x"))
      }
      "cbx version --verbose x" parse cbx { args =>
        args ==> Map("verbose" -> Seq("x"))
      }
      "cbx version --verbose=x" parse cbx { args =>
        args ==> Map("verbose" -> Seq("x"))
      }
      "cbx version --verbose=x=y" parse cbx { args =>
        args ==> Map("verbose" -> Seq("x=y"))
      }
      "cbx version --verbose=x=y,z=w" parse cbx { args =>
        args ==> Map("verbose" -> Seq("x=y,z=w"))
      }
      "cbx version --verbose x=y" parse cbx { args =>
        args ==> Map("verbose" -> Seq("x=y"))
      }
      "cbx version --verbose x=y z=w".fail(cbx(_ => ()), "too many arguments")
    }
    "required argument optional" - {
      "cbx run" parse cbx { _ ==> Map.empty } // make sure it works first
      "cbx run -f x" parse cbx { _ ==> Map("file" -> Seq("x")) }
      "cbx run --file x" parse cbx { _ ==> Map("file" -> Seq("x")) }
      "cbx run --file=x" parse cbx { _ ==> Map("file" -> Seq("x")) }
      "cbx run --file=x=y,z=w" parse cbx { _ ==> Map("file" -> Seq("x=y,z=w")) }
      "cbx run --file".fail(cbx(_ => ()), "requires")
      "cbx run --file --".fail(cbx(_ => ()), "requires")
    }
    "no argument optional" - {
      "cbx run --force=x".fail(cbx(_ => ()), "no argument allowed")
      "cbx run --force x" parse cbx { _ ==> Map("force" -> Seq(""), "pipeline" -> Seq("x")) }
    }
    "global optional" - {
      "cbx --server run run" parse cbx {_ ==> Map("server" -> Seq("run"))}
      "cbx -s run run" parse cbx {_ ==> Map("server" -> Seq("run"))}
      "cbx --server=run run" parse cbx {_ ==> Map("server" -> Seq("run"))}
      "cbx -x run".fail(cbx(_ => ()), "unknown option")
      "cbx --x run".fail(cbx(_ => ()), "unknown option")
    }
    "positional" - {
      "cbx login x" parse cbx { _ ==> Map("server_url" -> Seq("x"))}
      "cbx login x y" parse cbx { _ ==> Map("server_url" -> Seq("x"), "username" -> Seq("y"))}
      "cbx login x y z" parse cbx { _ ==> Map("server_url" -> Seq("x"), "username" -> Seq("y"), "password" -> Seq("z"))}
      "cbx login - x y z".fail(cbx(_ => ()), "too many")
      "cbx login - y" parse cbx { _ ==> Map("server_url" -> Seq("-"), "username" -> Seq("y"))}
    }
    "out of order options" - {
      "cbx run --force pipelinename -f x" parse cbx {
        _ ==> Map("force" -> Seq(""), "pipeline" -> Seq("pipelinename"), "file" -> Seq("x"))
      }
      "cbx run --force -- -f" parse cbx {
        _ ==> Map("force" -> Seq(""), "pipeline" -> Seq("-f"))
      }
      "cbx run --force -- --file" parse cbx {
        _ ==> Map("force" -> Seq(""), "pipeline" -> Seq("--file"))
      }
      "cbx run --force -- --" parse cbx {
        _ ==> Map("force" -> Seq(""), "pipeline" -> Seq("--"))
      }
      "cbx run --force -- -f x".fail(cbx(_ => ()), "too many")
    }
    "nested1" - {
      "cbx level1 level2-1 x=y level3 z" parse cbx {
        _ ==> Map("p2" -> Seq("x=y"), "p3" -> Seq("z"))
      }
    }
    "nested2" - {
      "cbx level1 level2-2 --" parse cbx {
        _ ==> Map.empty
      }
    }
  }
}
