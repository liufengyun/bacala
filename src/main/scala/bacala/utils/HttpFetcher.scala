package bacala.util

import scalaj.http._

/** Fetches the http or https resource via a specified URL
  */
object HttpFetcher {
  def get(url: String) = {
    println("Downloading " + url)
    val response = Http(url).asString

    if (response.code == 200) Some(response.body) else {
      println("Error: failed to download " + url)
      None
    }
  }
}
