package bacala.util

import bacala.core._

/** Constructs a dependency tree from solution set
  */
trait DependencyTree { this: Repository =>
  type TreeT = Tree[PackageT, DependencyEdge[PackageT, DependencyT]]

  def buildTree(solution: Set[PackageT]): TreeT = {
    def find(dep: DependencyT) = solution.find(_.lib == dep.lib).get
    def build(pkg: PackageT, path: Seq[PackageT]): TreeT = {
      val root: TreeT = Node(pkg)
      (this.apply(pkg) :\ root) { case ((dep, _), tree) =>
        val child = find(dep)
        val edge = HealthyEdge(dep, child)

        if (path.contains(child))
          tree + (edge, Leaf)
        else
          tree + (edge, build(child, pkg +: path))
      }
    }

    build(this.root, Seq())
  }
}
