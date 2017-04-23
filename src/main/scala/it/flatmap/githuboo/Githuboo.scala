package it.flatmap.githuboo

import java.nio.charset.StandardCharsets.UTF_8

import gigahorse.{HeaderNames, MimeTypes, Request}
import gigahorse.support.asynchttpclient.Gigahorse
import spray.json._

sealed trait GithubooCommand

case class CreateProject(
name: String,
description: String,
homepage: String,
`private`: Boolean = false,
has_issues: Boolean = true,
has_projects: Boolean = false,
has_wiki: Boolean = true,
auto_init: Boolean = false,
gitignore_template: String = "Scala",
license_template: String = "mit",
allow_squash_merge: Boolean = true,
allow_merge_commit: Boolean = true,
allow_rebase_merge: Boolean = true) extends GithubooCommand
case class DeleteProject() extends GithubooCommand

object CreateProjectProtocol extends DefaultJsonProtocol {
  implicit val commandFormat = jsonFormat13(CreateProject)
}


sealed trait GithubooEvent
case class ProjectCreated(name: String) extends GithubooEvent
case class ProjectDeleted(name: String) extends GithubooEvent

/*Requests builder*/
abstract class RequestBuilder {
  protected val baseUrl = "https://api.github.com"
  def build: Request
}

case class CreateRepo(bodyPayload: CreateProject, name: String) extends RequestBuilder {
  import CreateProjectProtocol._
  def build = Gigahorse.url(s"$baseUrl/user/repos").post(bodyPayload.toJson.prettyPrint, UTF_8)
}

case class DeleteRepo(owner: String, repoName: String) extends RequestBuilder {
  def build = Gigahorse.url(s"$baseUrl/repos/$owner/$repoName").delete
}

/**
  * API Clients
  */
abstract class AbstractClient {
  def httpHeaders: Map[String, String] =
    Map(HeaderNames.ACCEPT -> MimeTypes.JSON)
  def complete(request: Request): Request =
    if (httpHeaders.isEmpty) request
    else request.addHeaders(httpHeaders.toList: _*)
  def apply(builder: RequestBuilder): Request =
    complete(builder.build)
}
case class NoAuthClient() extends AbstractClient

case class OAuthClient(token: String) extends AbstractClient {
  override def httpHeaders: Map[String, String] =
    super.httpHeaders ++ Map("Authorization" -> "bearer %s".format(token))
  override def toString: String = s"OAuthClient(****)"
}

object OAuthClient {
  def apply(token: String): OAuthClient = OAuthClient(token)
}

trait Githuboo {
  def createGitHubProject(name: String): Option[ProjectCreated] = {None}
  def deleteGitHubProject(name: String): Option[ProjectDeleted] = {None}
}

object Main extends App {
  import CreateProjectProtocol._
  println(CreateProject(name="Pippo", description="Pluto", homepage = "Topolino").toJson.prettyPrint)
}
