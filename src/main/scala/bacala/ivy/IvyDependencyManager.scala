package bacala.ivy

object IvyDependencyManager {
  def run(setting: String, file: String) = {
    val parser = new IvyParser(setting)
    println(parser.parse("file://" + file))
  }

  def main(args: Array[String]) = {
    if (args.length == 1)
      run("", args(0))
    else if (args.length == 2)
      run(args(0), args(1))
    else
      println("Usage: run [<setting xml>] <ivy xml>")
  }
}
