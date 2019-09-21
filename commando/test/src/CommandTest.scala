package commando

import utest._

object CommandTests extends TestSuite {

  def eval(cmd: Command, args: List[String]): Boolean = {
    cmd.parse(args).isEmpty
  }

  val tests = Tests {
    "empty" - {
      val cmd = new Command("cmd")
      assert(eval(cmd, Nil))
      assert(!eval(cmd, "a" :: Nil))
    }
    "positional required" - {
      val cmd = new Command("cmd") {
        positional("POS")
      }
      assert(!eval(cmd, Nil)) // no param
      assert(eval(cmd, "" :: Nil)) // empty param
      assert(eval(cmd, "a" :: Nil)) // one param
      assert(!eval(cmd, "a" :: "b" :: Nil)) // too many params
      assert(!eval(cmd, "a" :: "b" :: "c" :: Nil)) // too many params
    }
    "positional two" - {
      val cmd = new Command("cmd") {
        positional("one")
        positional("two")
      }
      assert(!eval(cmd, Nil)) // no param
      assert(!eval(cmd, "" :: Nil)) // empty param
      assert(!eval(cmd, "a" :: Nil)) // one param
      assert(eval(cmd, "a" :: "b" :: Nil)) // too many params
      assert(!eval(cmd, "a" :: "b" :: "c" :: Nil)) // too many params
    }
    "positional repeated" - {
      val cmd = new Command("cmd") {
        positional("one").repeat()
      }
      assert(eval(cmd, Nil))
      assert(eval(cmd, "" :: Nil))
      assert(eval(cmd, "a" :: Nil))
      assert(eval(cmd, "a" :: "b" :: Nil))
      assert(eval(cmd, "a" :: "b" :: "c" :: Nil))
    }
    "positional optional" - {
      val cmd = new Command("cmd") {
        positional("one").optional()
      }
      assert(eval(cmd, Nil))
      assert(eval(cmd, "" :: Nil))
      assert(eval(cmd, "a" :: Nil))
      assert(!eval(cmd, "a" :: "b" :: Nil))
      assert(!eval(cmd, "a" :: "b" :: "c" :: Nil))
    }
    "positional combination" - {
      class Cmd extends Command("cmd") {
        var one: Option[String] = None
        var two: Option[String] = None
        positional("one")
          .optional()
          .action(v => one = Some(v))
        positional("two")
          .repeat()
          .action(v => two = Some(v))
      }

      val cmd1 = new Cmd
      assert(eval(cmd1, Nil))
      assert(cmd1.one == None && cmd1.two == None)

      val cmd2 = new Cmd
      assert(eval(cmd2, "" :: Nil))
      assert(cmd2.one == Some("") && cmd2.two == None)

      val cmd3 = new Cmd
      assert(eval(cmd3, "a" :: Nil))
      assert(cmd3.one == Some("a") && cmd3.two == None)

      val cmd4 = new Cmd
      assert(eval(cmd4, "a" :: "b" :: Nil))
      assert(cmd4.one == Some("a") && cmd4.two == Some("b"))

      val cmd5 = new Cmd
      assert(eval(cmd5, "a" :: "b" :: "c" :: Nil))
      assert(cmd5.one == Some("a") && cmd5.two == Some("c"))
    }
    "named flag optional" - {
      val cmd = new Command("cmd") {
        named("param")
      }
      assert(eval(cmd, Nil))
      assert(!eval(cmd, "" :: Nil))
      assert(eval(cmd, "--param" :: Nil))
      assert(!eval(cmd, "--param" :: "" :: Nil))
      assert(!eval(cmd, "--param" :: "--param" :: Nil))
      assert(!eval(cmd, "--param" :: "--param" :: "a" :: Nil))
    }
    "named flag required" - {
      val cmd = new Command("cmd") {
        named("param").require()
      }
      assert(!eval(cmd, Nil))
      assert(!eval(cmd, "" :: Nil))
      assert(eval(cmd, "--param" :: Nil))
      assert(!eval(cmd, "--param" :: "" :: Nil))
      assert(!eval(cmd, "--param" :: "--param" :: Nil))
      assert(!eval(cmd, "--param" :: "--param" :: "a" :: Nil))
    }
    "named flag repeated" - {
      val cmd = new Command("cmd") {
        named("param").repeat()
      }
      assert(eval(cmd, Nil))
      assert(!eval(cmd, "" :: Nil))
      assert(eval(cmd, "--param" :: Nil))
      assert(!eval(cmd, "--param" :: "" :: Nil))
      assert(eval(cmd, "--param" :: "--param" :: Nil))
      assert(!eval(cmd, "--param" :: "--param" :: "a" :: Nil))
    }
    "named arg" - {
      class Cmd extends Command("cmd") {
        var one: Option[String] = None
        var two: Option[String] = None
        named("one")
          .arg("value")
          .action(v => one = Some(v))
        positional("two")
          .action(v => two = Some(v))
      }

      val cmd1 = new Cmd
      assert(!eval(cmd1, Nil))
      assert(cmd1.one == None && cmd1.two == None)

      val cmd2 = new Cmd
      assert(eval(cmd2, "--one=a" :: "b" :: Nil))
      assert(cmd2.one == Some("a") && cmd2.two == Some("b"))

      val cmd3 = new Cmd
      assert(eval(cmd3, "b" :: "--one=a" :: Nil))
      assert(cmd3.one == Some("a") && cmd3.two == Some("b"))

      val cmd4 = new Cmd
      assert(eval(cmd4, "--one" :: "a" :: "b" :: Nil))
      assert(cmd4.one == Some("a") && cmd4.two == Some("b"))

      val cmd5 = new Cmd
      assert(eval(cmd5, "b" :: "--one" :: "a" :: Nil))
      assert(cmd5.one == Some("a") && cmd5.two == Some("b"))

      val cmd6 = new Cmd
      assert(!eval(cmd6, "--one" :: "--a" :: "b" :: Nil))

      val cmd7 = new Cmd
      assert(!eval(cmd7, "b" :: "--one" :: Nil))

      val cmd8 = new Cmd
      assert(!eval(cmd8, "a" :: "--" :: "--one" :: Nil))

      val cmd9 = new Cmd
      assert(!eval(cmd9, "--one" :: "--" :: "--one" :: "b" :: Nil))

      val cmd10 = new Cmd
      assert(eval(cmd10, "--one=--ab" :: "b" :: Nil))
      assert(cmd10.one == Some("--ab") && cmd10.two == Some("b"))
    }
    "named arg optional" - {
      class Cmd extends Command("cmd") {
        var one: Option[String] = None
        var two: Option[String] = None
        named("one")
          .optionalArg("value")
          .action(v => one = v)
        positional("two")
          .action(v => two = Some(v))
      }

      val cmd1 = new Cmd
      assert(!eval(cmd1, Nil))
      assert(cmd1.one == None && cmd1.two == None)

      val cmd2 = new Cmd
      assert(eval(cmd2, "--one=a" :: "b" :: Nil))
      assert(cmd2.one == Some("a") && cmd2.two == Some("b"))

      val cmd3 = new Cmd
      assert(eval(cmd3, "b" :: "--one=a" :: Nil))
      assert(cmd3.one == Some("a") && cmd3.two == Some("b"))

      val cmd4 = new Cmd
      assert(eval(cmd4, "--one" :: "a" :: "b" :: Nil))
      assert(cmd4.one == Some("a") && cmd4.two == Some("b"))

      val cmd5 = new Cmd
      assert(eval(cmd5, "b" :: "--one" :: "a" :: Nil))
      assert(cmd5.one == Some("a") && cmd5.two == Some("b"))

      val cmd6 = new Cmd
      assert(!eval(cmd6, "--one" :: "--a" :: "b" :: Nil))

      val cmd7 = new Cmd
      assert(eval(cmd7, "b" :: "--one" :: Nil))
      assert(cmd7.one == None && cmd7.two == Some("b"))

      val cmd8 = new Cmd
      assert(eval(cmd8, "--" :: "--one" :: Nil))
      assert(cmd8.one == None && cmd8.two == Some("--one"))

      val cmd9 = new Cmd
      assert(eval(cmd9, "--one" :: "a" :: "--" :: "--one" :: Nil))
      assert(cmd9.one == Some("a") && cmd9.two == Some("--one"))

      val cmd10 = new Cmd
      assert(eval(cmd10, "--one=--ab" :: "b" :: Nil))
      assert(cmd10.one == Some("--ab") && cmd10.two == Some("b"))
    }
  }
}
