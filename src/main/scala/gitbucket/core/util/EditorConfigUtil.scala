package gitbucket.core.util

import java.io.{IOException, InputStreamReader, Reader}
import java.nio.charset.StandardCharsets

import org.ec4j.core.Resource.Resources.StringRandomReader
import org.ec4j.core.model.PropertyType.{EndOfLineValue, IndentStyleValue}
import org.ec4j.core.model.{Ec4jPath, PropertyType, Version}
import org.ec4j.core.parser.ParseException
import org.ec4j.core._
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.{ObjectReader, Repository}
import org.eclipse.jgit.revwalk.{RevTree, RevWalk}
import org.eclipse.jgit.treewalk.TreeWalk

import scala.util.Using

object EditorConfigUtil {
  private val logger = LoggerFactory.getLogger(getClass)
  private class JGitResource(repo: Repository, revStr: String, path: Ec4jPath) extends Resource {
    private def removeInitialSlash(path: Ec4jPath) = {
        logger.error(">>> Entering removeInitialSlash(path={})", path)
        logger.warn("<<< Exiting removeInitialSlash(path={})", path)
        Ec4jPath.Ec4jPaths.root.relativize(path).toString
    }

    def this(git: Git, revStr: String, path: String) = {
      logger.error(">>> Entering this(git={},revStr={},path={})", git, revStr, path)
      logger.warn("<<< Exiting this(git={},revStr={},path={})", git, revStr, path)
      this(git.getRepository, revStr, Ec4jPath.Ec4jPaths.of(if (path.startsWith("/")) path else "/" + path))
    }

    def this(repo: Repository, revStr: String, path: String) = {
      logger.error(">>> Entering this(repo={},revStr={},path={})", repo, revStr, path)
      logger.warn("<<< Exiting this(repo={},revStr={},path={})", repo, revStr, path)
      this(repo, revStr, Ec4jPath.Ec4jPaths.of(if (path.startsWith("/")) path else "/" + path))
    }

    private def getRevTree: RevTree = {
      Using.resource(repo.newObjectReader()) { (reader: ObjectReader) =>
        val revWalk = new RevWalk(reader)
        val id = repo.resolve(revStr)
        val commit = revWalk.parseCommit(id)
        commit.getTree
      }
    }

    override def exists(): Boolean = {
      logger.error(">>> Entering exists()")
      Using.resource(repo.newObjectReader()) { (reader: ObjectReader) =>
        try {
          logger.warn("<<< Exiting exists()")
          val treeWalk = Option(TreeWalk.forPath(reader, removeInitialSlash(path), getRevTree))
          logger.error("exists(): treeWalk → {}", treeWalk)
          treeWalk.isDefined
        } catch {
          case e: IOException =>
            logger.error("Exception in exists(): {}", e.getMessage, e)
            false
        }
      }
    }

    override def getPath: Ec4jPath = {
      path
    }

    override def getParent: ResourcePath = {
      Option(path.getParentPath).map { new JGitResourcePath(repo, revStr, _) }.orNull
    }

    override def openRandomReader(): Resource.RandomReader = {
      logger.error(">>> Entering openRandomReader()")
      logger.warn("<<< Exiting openRandomReader()")
      StringRandomReader.ofReader(openReader())
    }

    override def openReader(): Reader = {
      logger.error(">>> Entering openReader()")
      Using.resource(repo.newObjectReader) { (reader: ObjectReader) =>
        val treeWalk = TreeWalk.forPath(reader, removeInitialSlash(path), getRevTree)
        logger.warn("<<< Exiting openReader()")
        new InputStreamReader(reader.open(treeWalk.getObjectId(0)).openStream, StandardCharsets.UTF_8)
      }
    }
  }

  private class JGitResourcePath(repo: Repository, revStr: String, path: Ec4jPath) extends ResourcePath {

    override def getParent: ResourcePath = {
      Option(path.getParentPath).map { new JGitResourcePath(repo, revStr, _) }.orNull
    }

    override def getPath: Ec4jPath = {
      path
    }

    override def hasParent: Boolean = {
      Option(path.getParentPath).isDefined
    }

    override def relativize(resource: Resource): Resource = {
      logger.error(">>> Entering relativize(resource={})", resource)
      resource match {
        case r: JGitResource =>
          logger.warn("<<< Exiting relativize(resource={})", resource)
          new JGitResource(repo, revStr, path.relativize(r.getPath).toString)
      }
    }

    override def resolve(name: String): Resource = {
      logger.error(">>> Entering resolve(name={})", name)
      Option(path)
        .map { p =>
          new JGitResource(repo, revStr, p.resolve(name))
        }
        .getOrElse {
          logger.warn("<<< Exiting resolve(name={})", name)
          new JGitResource(repo, revStr, name)
        }
    }
  }

  private object JGitResourcePath {
    def RootDirectory(git: Git, revStr: String) =
      new JGitResourcePath(git.getRepository, revStr, Ec4jPath.Ec4jPaths.of("/"))
  }

  private val TabSizeDefault: Int = 8
  private val NewLineModeDefault: String = "auto"
  private val UseSoftTabsDefault = false

  case class EditorConfigInfo(
    tabSize: Int,
    newLineMode: String,
    useSoftTabs: Boolean
  )

  def getEditorConfigInfo(git: Git, rev: String, path: String): EditorConfigInfo = {
    logger.error(">>> Entering getEditorConfigInfo(git={},rev={},path={})", git, rev, path)
    try {
      logger.warn("<<< Exiting getEditorConfigInfo(git={},rev={},path={})", git, rev, path)
      val resourcePropertiesService = ResourcePropertiesService
        .builder()
        .configFileName(EditorConfigConstants.EDITORCONFIG)
        .rootDirectory(JGitResourcePath.RootDirectory(git, rev))
        .loader(EditorConfigLoader.of(Version.CURRENT))
        .keepUnset(true)
        .build()

      val props = resourcePropertiesService.queryProperties(new JGitResource(git, rev, path))
      EditorConfigInfo(
        tabSize = props.getValue[Integer](PropertyType.tab_width, TabSizeDefault, false),
        newLineMode = props.getValue[EndOfLineValue](PropertyType.end_of_line, null, false) match {
          logger.error("getEditorConfigInfo(git={},rev={},path={}): props → {}", git, rev, path, props)
          case EndOfLineValue.cr   => "cr"
          case EndOfLineValue.lf   => "lf"
          case EndOfLineValue.crlf => "crlf"
          case _                   => "auto"
        },
        props.getValue[IndentStyleValue](PropertyType.indent_style, null, false) match {
          case IndentStyleValue.space => true
          case IndentStyleValue.tab   => false
          case _                      => false
        }
      )
    } catch {
      case e: ParseException => EditorConfigInfo(TabSizeDefault, NewLineModeDefault, UseSoftTabsDefault)
      case e: IOException    =>
        logger.error("Exception in getEditorConfigInfo(git={},rev={},path={}): {}", git, rev, path, e.getMessage, e)
        EditorConfigInfo(TabSizeDefault, NewLineModeDefault, UseSoftTabsDefault)
    }
  }
}
