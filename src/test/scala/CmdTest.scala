package cmd

import utest._

object CmdTests extends TestSuite {

  val cbx = cmd.Command(
    "cbx",
    cmd.Option("server", Some('s'), Some(cmd.Parameter("name"))),
    cmd.Command(
      "version",
      cmd.Option("verbose", Some('v'), Some(cmd.Parameter("k=v", false)))),
    cmd.Command("login",
                cmd.Parameter("server_url"),
                cmd.Parameter("username", false),
                cmd.Parameter("password", false)),
    cmd.Command("run",
                cmd.Option("file", Some('f'), Some(cmd.Parameter("file_name"))),
                cmd.Option("force", None),
                cmd.Parameter("pipeline", false)),
    cmd.Command("level1",
                cmd.Command("level2-1",
                            cmd.Parameter("p2"),
                            cmd.Command("level3", cmd.Parameter("p3"))),
                cmd.Command("level2-2"))
  )

  def parse(in: String): CommandLine = cmd.parse(cbx, in.split(" ").tail) match {
    case Left(ex) => throw ex
    case Right(res) => res
  }

  def shouldFail(in: String) =
    assert(cmd.parse(cbx, in.split(" ").tail).isLeft)

  val tests = Tests {
    "printUsage" - {
      cbx.usage
    }
    "simple" - {
      assert(
        parse("cbx version").subcommand.get == CommandLine("version",
                                                           Map.empty,
                                                           None))
    }
    "emptyAllowedOption" - {
      assert(
        parse("cbx version -v").subcommand.get == CommandLine(
          "version",
          Map("verbose" -> ""),
          None))
      assert(
        parse("cbx version --verbose").subcommand.get == CommandLine(
          "version",
          Map("verbose" -> ""),
          None))
    }
    "setAllowedOption" - {
      assert(
        parse("cbx version -v x").subcommand.get == CommandLine(
          "version",
          Map("verbose" -> "x"),
          None))
      assert(
        parse("cbx version --verbose x").subcommand.get == CommandLine(
          "version",
          Map("verbose" -> "x"),
          None))
      assert(
        parse("cbx version --verbose=x").subcommand.get == CommandLine(
          "version",
          Map("verbose" -> "x"),
          None))
      assert(
        parse("cbx version --verbose=x=y").subcommand.get == CommandLine(
          "version",
          Map("verbose" -> "x=y"),
          None))
      assert(
        parse("cbx version --verbose=x=y,z=w").subcommand.get == CommandLine(
          "version",
          Map("verbose" -> "x=y,z=w"),
          None))
      assert(
        parse("cbx version --verbose x=y,z=w").subcommand.get == CommandLine(
          "version",
          Map("verbose" -> "x=y,z=w"),
          None))
      shouldFail("cbx version --verbose x=y z=w")
    }
    "requiredArgOption" - {
      assert(parse("cbx run").subcommand.get == CommandLine("run", Map(), None)) // make sure it works first
      assert(
        parse("cbx run -f x").subcommand.get == CommandLine("run",
                                                            Map("file" -> "x"),
                                                            None))
      assert(
        parse("cbx run --file x").subcommand.get == CommandLine(
          "run",
          Map("file" -> "x"),
          None))
      assert(
        parse("cbx run --file=x").subcommand.get == CommandLine(
          "run",
          Map("file" -> "x"),
          None))
      assert(
        parse("cbx run --file=x=y,z=w").subcommand.get == CommandLine(
          "run",
          Map("file" -> "x=y,z=w"),
          None))
      shouldFail("cbx run --file")
      shouldFail("cbx run --file --")
    }
    "noArgOption" - {
      shouldFail("cbx run --force=x")
      assert(
        parse("cbx run --force x").subcommand.get == CommandLine(
          "run",
          Map("force" -> "", "pipeline" -> "x"),
          None))
    }
    "globalOption" - {
      assert(parse("cbx --server run run").arguments == Map("server" -> "run"))
      assert(
        parse("cbx --server run run").subcommand.get == CommandLine("run",
                                                                    Map.empty,
                                                                    None))
      assert(parse("cbx -s run run").arguments == Map("server" -> "run"))
      assert(
        parse("cbx -s run run").subcommand.get == CommandLine("run",
                                                              Map.empty,
                                                              None))
      assert(parse("cbx --server=run run").arguments == Map("server" -> "run"))
      assert(
        parse("cbx --server=run run").subcommand.get == CommandLine("run",
                                                                    Map.empty,
                                                                    None))
      shouldFail("cbx -x run")
      shouldFail("cbx --x run")
    }
    "parameter" - {
      assert(
        parse("cbx login x").subcommand.get == CommandLine(
          "login",
          Map("server_url" -> "x"),
          None))
      assert(
        parse("cbx login x y").subcommand.get == CommandLine(
          "login",
          Map("server_url" -> "x", "username" -> "y"),
          None))
      assert(
        parse("cbx login x y z").subcommand.get == CommandLine(
          "login",
          Map("server_url" -> "x", "username" -> "y", "password" -> "z"),
          None))
      shouldFail("cbx login - y z w")
      assert(
        parse("cbx login - y").subcommand.get == CommandLine(
          "login",
          Map("server_url" -> "-", "username" -> "y"),
          None))
    }
    "outOfOrderOptions" - {
      assert(
        parse("cbx run --force pipelinename -f x").subcommand.get == CommandLine(
          "run",
          Map("force" -> "", "pipeline" -> "pipelinename", "file" -> "x"),
          None))
      assert(
        parse("cbx run --force -- -f").subcommand.get == CommandLine(
          "run",
          Map("force" -> "", "pipeline" -> "-f"),
          None))
      assert(
        parse("cbx run --force -- --file").subcommand.get == CommandLine(
          "run",
          Map("force" -> "", "pipeline" -> "--file"),
          None))
      assert(
        parse("cbx run --force -- --").subcommand.get == CommandLine(
          "run",
          Map("force" -> "", "pipeline" -> "--"),
          None))
      shouldFail("cbx run --force -- -f x") // too many parameters
    }
    "nested1" - {
      val line = parse("cbx level1 level2-1 x=y level3 z").subcommand.get
      val expected = CommandLine(
        "level1",
        Map.empty,
        Some(
          CommandLine("level2-1",
                      Map("p2" -> "x=y"),
                      Some(CommandLine("level3", Map("p3" -> "z"), None)))))
      assert(line == expected)
    }
    "nested2" - {
      val line = parse("cbx level1 level2-2 --").subcommand.get
      val expected = CommandLine("level1",
                                 Map.empty,
                                 Some(CommandLine("level2-2", Map.empty, None)))
      assert(line == expected)
    }
  }
}
