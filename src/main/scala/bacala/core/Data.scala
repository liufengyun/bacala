package bacala.core

/** This file defines the common entities for the Java world(Ivy and Maven)
  */

case class JLib(groupId: String, artifactId: String) extends Lib {
  override def id =  groupId + ":" + artifactId

  override def toString = id
}

case class JPackage(lib: JLib, version:String) extends Package {
  type LibT = JLib

  def artifactId = lib.artifactId
  def groupId = lib.groupId
}