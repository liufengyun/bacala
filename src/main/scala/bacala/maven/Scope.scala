package bacala.maven

/**
  *  scope definitions for dependencies in POM
  */
object Scope extends Enumeration {
  type Scope = Value

  val COMPILE  = Value("compile")
  val RUNTIME = Value("runtime")
  val TEST = Value("test")
  val SYSTEM = Value("system")
  val PROVIDED = Value("provided")

  def apply(s: String): Scope = {
    this.values.find(v => v.toString().toLowerCase() == s.toLowerCase()) match {
      case Some(scope) => scope
      case None => {
        throw new UnsupportedOperationException("Scope " + s + " is not supported.")
      }
    }
  }
}
