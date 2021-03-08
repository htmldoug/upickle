package ujson

import java.nio.file.{Files, Paths}

import utest._

object ExampleJsonTests extends TestSuite {
  def check(name: String) = {
    TestUtil.checkParse(new String(Files.readAllBytes(Paths.get("exampleJson", name))), true)
  }
  val tests = Tests {
    test - check("australia-abc.json")
    test - check("bitcoin.json")
    test - check("doj-blog.json")
    test - check("eu-lobby-country.json")
    test - check("eu-lobby-financial.json")
    test - check("eu-lobby-repr.json")
    test - check("github-events.json")
    test - check("github-gists.json")
    test - check("json-generator.json")
    test - check("meteorites.json")
    test - check("movies.json")
    test - check("reddit-scala.json")
    test - check("rick-morty.json")
    test - check("temp-anomaly.json")
    test - check("thai-cinemas.json")
    test - check("turkish.json")
    test - check("twitter_api_compact_response.json")
    test - check("twitter_api_response.json")
  }
}
