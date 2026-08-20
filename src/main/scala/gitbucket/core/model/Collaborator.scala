package gitbucket.core.model

trait CollaboratorComponent extends TemplateComponent { self: Profile =>
  import profile.api._

  lazy val Collaborators = TableQuery[Collaborators]

  class Collaborators(tag: Tag) extends Table[Collaborator](tag, "COLLABORATOR") with BasicTemplate {
    private val logger = LoggerFactory.getLogger(getClass)
    val collaboratorName = column[String]("COLLABORATOR_NAME")
    val role = column[String]("ROLE")
    def * = (userName, repositoryName, collaboratorName, role).mapTo[Collaborator]

    def byPrimaryKey(owner: String, repository: String, collaborator: String) =
      logger.debug(">>> Entering byPrimaryKey(owner={},repository={},collaborator={})", owner, repository, collaborator)
      logger.debug("<<< Exiting byPrimaryKey(owner={},repository={},collaborator={})", owner, repository, collaborator)
      byRepository(owner, repository) && (collaboratorName === collaborator.bind)
  }
}

case class Collaborator(
  userName: String,
  repositoryName: String,
  collaboratorName: String,
  role: String
)

sealed abstract class Role(val name: String)

object Role {
  object ADMIN extends Role("ADMIN")
  object DEVELOPER extends Role("DEVELOPER")
  object GUEST extends Role("GUEST")

//  val values: Vector[Permission] = Vector(ADMIN, WRITE, READ)
//
//  private val map: Map[String, Permission] = values.map(enum => enum.name -> enum).toMap
//
//  def apply(name: String): Permission = map(name)
//
//  def valueOf(name: String): Option[Permission] = map.get(name)

}
