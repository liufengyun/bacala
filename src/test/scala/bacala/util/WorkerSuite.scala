package bacala.test.util

import bacala.test.BasicSuite
import bacala.util._

class WorkerSuite extends BasicSuite {
  test("chain two workers") {
    val worker1 = new Worker[Int, Int] {
      override def apply(x: Int) = if (x % 2 == 0) Some(x) else None
    }
    val worker2 = new Worker[Int, Int] {
      override def apply(x: Int) = if (x % 2 != 0) Some(x) else None
    }
    val worker3 = worker1 or worker2

    assert(worker1(1) === None)
    assert(worker1(2) === Some(2))

    assert(worker2(1) === Some(1))
    assert(worker2(2) === None)

    assert(worker3(1) === Some(1))
    assert(worker3(2) === Some(2))
  }
}

class CachedWorkerSuite extends BasicSuite {
  test("chain worker with cached worker") {
    val worker1 = new Worker[Int, String] {
      override def apply(x: Int) = if (x % 5 == 1) Some("good") else None
    }

    val worker2 = new Worker[Int, String] {
      override def apply(x: Int) = if (x % 5 == 2) Some("good") else None
    }

    val worker3 = new Worker[Int, String] {
      override def apply(x: Int) = if (x % 5 == 3) Some("good") else None
    }

    val cachedWorker = new CachedWorker[Int, String] {
      override val cache = new MemoryCache[Int, Option[String]] {}
    } or worker1

    val worker4 = cachedWorker or worker2
    val worker5 = worker4 or worker3

    assert(cachedWorker(1) === Some("good"))
    assert(cachedWorker(2) === None)
    assert(cachedWorker.cache.fetch(1, Some("")) === Some("good"))
    assert(cachedWorker.cache.fetch(2, Some("")) === None)

    assert(worker4(1) === Some("good"))
    assert(worker4(2) === Some("good"))
    assert(cachedWorker.cache.fetch(1, Some("")) === Some("good"))
    assert(cachedWorker.cache.fetch(2, Some("")) === Some("good"))

    assert(worker5(1) === Some("good"))
    assert(worker5(2) === Some("good"))
    assert(worker5(3) === Some("good"))
    assert(cachedWorker.cache.fetch(1, Some("")) === Some("good"))
    assert(cachedWorker.cache.fetch(2, Some("")) === Some("good"))
    assert(cachedWorker.cache.fetch(3, Some("")) === Some("good"))
  }
}
