package it.flatmap.githuboo

import java.nio.charset.StandardCharsets.UTF_8

import gigahorse.{FullResponse, HeaderNames, MimeTypes, Request}
import gigahorse.support.asynchttpclient.Gigahorse
import spray.json._

import scala.concurrent.Await
import scala.concurrent.duration._

sealed trait GithubooCommand

case class CreateProject(
name: String,
description: String,
homepage: String,
`private`: Boolean = false,
has_issues: Boolean = true,
has_projects: Boolean = false,
has_wiki: Boolean = true,
auto_init: Boolean = true,
gitignore_template: String = "Scala",
license_template: String = "mit",
allow_squash_merge: Boolean = true,
allow_merge_commit: Boolean = true,
allow_rebase_merge: Boolean = true) extends GithubooCommand

case class DeleteProject() extends GithubooCommand

object CreateProjectProtocol extends DefaultJsonProtocol {
  implicit val commandFormat = jsonFormat13(CreateProject)
}

object JSONUtility {

  trait Serializer[T] {
    def serialize(t: T): String
  }

  object Serializer {
    implicit object CreateProjectSerializer extends Serializer[CreateProject] {
      def serialize(createProject: CreateProject): String = {
        import CreateProjectProtocol._
        createProject.toJson.prettyPrint
      }
    }
  }
}

object JsonSerializer {
  import JSONUtility.Serializer
  def serialize[T: Serializer](t: T): String = {
    val engine = implicitly[Serializer[T]]
    engine.serialize(t)
  }
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

case class OAuthClient(token: String) extends AbstractClient {
  override def httpHeaders: Map[String, String] =
    super.httpHeaders ++ Map("Authorization" -> "bearer %s".format(token))
  override def toString: String = s"OAuthClient(****)"
}

trait Githuboo {

  def client: AbstractClient

  private def runOperation[A](request: RequestBuilder): FullResponse = {
    Gigahorse.withHttp(Gigahorse.config) { http =>
      val f = http.run(client(request))
      Await.result(f, 20.seconds)
    }
  }

  def createGitHubProject(bodyPayload: CreateProject): Option[ProjectCreated] = {
    val response = runOperation(CreateRepo(bodyPayload, bodyPayload.name))
    response.status match {
      case 201 => Some(ProjectCreated(response.header("Location").getOrElse("Invalid Location")))
      case _ => None
    }
  }

  def deleteGitHubProject(projectOwner: String, projectName: String): Option[ProjectDeleted] = {
    val response = runOperation(DeleteRepo(projectOwner, projectName))
    response.status match {
      case 204 => Some(ProjectDeleted(projectName))
      case _ => None
    }
  }
}

object Main extends App with Githuboo {
  val client = OAuthClient(System.getenv("GITHUB_TOKEN"))
  deleteGitHubProject("P3trur0", "Pippo")
}


