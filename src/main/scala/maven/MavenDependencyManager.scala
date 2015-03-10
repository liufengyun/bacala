package bacala.maven

import bacala.core._

class MavenDependencyManager extends DependencyManager {
  type Repo = MavenRepository

  override def resolve(repository: Repo): Result = ???
}
