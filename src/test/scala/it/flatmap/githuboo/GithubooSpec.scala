package it.flatmap.githuboo

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by ubaldo on 27/04/17.
  */
class GithubooSpec extends FlatSpec with Matchers {

  "A CreateProject instance" should "be pretty printed as valid JSON" in {

    println(JsonSerializer.serialize(
    CreateProject(name="Pippo",
      description="Pluto",
      homepage = "Topolino")))
  }
}
