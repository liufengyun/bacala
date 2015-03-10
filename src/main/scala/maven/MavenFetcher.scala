package bacala.maven

class MavenFetcher extends (MavenPackage => Set[Set[MavenPackage]]) {
  override def apply(p: MavenPackage) = ???
}
