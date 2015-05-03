package bacala.ivy

object IvyDependencyManager {
  def run(file: String) = {
    println(IvyParser("file://" + file))
  }

  def main(args: Array[String]) = {
    if (args.length != 1)
      println("should indicate an Ivy XML file")
    else
      run(args(0))
  }
}
