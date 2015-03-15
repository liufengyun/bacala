package bacala.maven

import bacala._

class MavenDependencyManager extends DependencyManager {
  type Repo = MavenRepository
  type Result = Set[MavenPackage]

  override def resolve(repository: Repo): Result = ???
}
