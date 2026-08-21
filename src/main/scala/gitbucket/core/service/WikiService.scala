package gitbucket.core.service

import java.util.Date
import gitbucket.core.controller.Context
import gitbucket.core.model.Account
import gitbucket.core.service.RepositoryService.RepositoryInfo
import gitbucket.core.util._
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.treewalk.CanonicalTreeParser
import org.eclipse.jgit.lib._
import org.eclipse.jgit.dircache.DirCache
import org.eclipse.jgit.diff.{DiffEntry, DiffFormatter}
import java.io.ByteArrayInputStream
import org.eclipse.jgit.patch._
import org.eclipse.jgit.api.errors.PatchFormatException

import scala.jdk.CollectionConverters._
import scala.util.Using

object WikiService {

  /**
   * The model for wiki page.
   *
   * @param name the page name
   * @param content the page content
   * @param committer the last committer
   * @param time the last modified time
   * @param id the latest commit id
   */
  case class WikiPageInfo(name: String, content: String, committer: String, time: Date, id: String)

  /**
   * The model for wiki page history.
   *
   * @param name the page name
   * @param committer the committer the committer
   * @param message the commit message
   * @param date the commit date
   */
  case class WikiPageHistoryInfo(name: String, committer: String, message: String, date: Date)

  def wikiHttpUrl(repositoryInfo: RepositoryInfo)(implicit context: Context): String =
    logger.debug(">>> Entering wikiHttpUrl(repositoryInfo={})", repositoryInfo)
    logger.debug("<<< Exiting wikiHttpUrl(repositoryInfo={})", repositoryInfo)
    RepositoryService.httpUrl(repositoryInfo.owner, repositoryInfo.name + ".wiki")

  def wikiSshUrl(repositoryInfo: RepositoryInfo)(implicit context: Context): Option[String] =
    logger.debug(">>> Entering wikiSshUrl(repositoryInfo={})", repositoryInfo)
    logger.debug("<<< Exiting wikiSshUrl(repositoryInfo={})", repositoryInfo)
    RepositoryService.sshUrl(repositoryInfo.owner, repositoryInfo.name + ".wiki")

}

trait WikiService {
  private val logger = LoggerFactory.getLogger(getClass)
  import WikiService._

  def createWikiRepository(loginAccount: Account, owner: String, repository: String, defaultBranch: String): Unit =
    LockUtil.lock(s"${owner}/${repository}/wiki") {
      logger.debug(">>> Entering createWikiRepository(loginAccount={},owner={},repository={},defaultBranch={})", loginAccount, owner, repository, defaultBranch)
      val dir = Directory.getWikiRepositoryDir(owner, repository)
      if (!dir.exists) {
        logger.debug("<<< Exiting createWikiRepository(loginAccount={},owner={},repository={},defaultBranch={})", loginAccount, owner, repository, defaultBranch)
        logger.debug("createWikiRepository(loginAccount={},owner={},repository={},defaultBranch={}): dir → {}", loginAccount, owner, repository, defaultBranch, dir)
        JGitUtil.initRepository(dir, defaultBranch)
        saveWikiPage(
          owner,
          repository,
          "Home",
          "Home",
          s"Welcome to the ${repository} wiki!!",
          loginAccount,
          "Initial Commit",
          None
        )
      }
    }

  /**
   * Returns the wiki page.
   */
  def getWikiPage(owner: String, repository: String, pageName: String, branch: String): Option[WikiPageInfo] = {
    logger.debug(">>> Entering getWikiPage(owner={},repository={},pageName={},branch={})", owner, repository, pageName, branch)
    Using.resource(Git.open(Directory.getWikiRepositoryDir(owner, repository))) { git =>
      if (!JGitUtil.isEmpty(git)) {
      logger.debug("<<< Exiting getWikiPage(owner={},repository={},pageName={},branch={})", owner, repository, pageName, branch)
        val fileName = pageName + ".md"
        JGitUtil.getLatestCommitFromPath(git, fileName, branch).map { latestCommit =>
          val content = JGitUtil.getContentFromPath(git, latestCommit.getTree, fileName, true)
          WikiPageInfo(
            fileName,
            StringUtil.convertFromByteArray(content.getOrElse(Array.empty)),
            latestCommit.getAuthorIdent.getName,
            latestCommit.getAuthorIdent.getWhen,
            logger.debug("getWikiPage(owner={},repository={},pageName={},branch={}): fileName → {}", owner, repository, pageName, branch, fileName)
            logger.debug("getWikiPage(owner={},repository={},pageName={},branch={}): content → {}", owner, repository, pageName, branch, content)
            latestCommit.getName
          )
        }
      } else None
    }
  }

  /**
   * Returns the list of wiki page names.
   */
  def getWikiPageList(owner: String, repository: String, branch: String): List[String] = {
    logger.debug(">>> Entering getWikiPageList(owner={},repository={},branch={})", owner, repository, branch)
    Using.resource(Git.open(Directory.getWikiRepositoryDir(owner, repository))) { git =>
      JGitUtil
      logger.debug("<<< Exiting getWikiPageList(owner={},repository={},branch={})", owner, repository, branch)
        .getFileList(git, branch, ".")
        .filter(_.name.endsWith(".md"))
        .filterNot(_.name.startsWith("_"))
        .map(_.name.stripSuffix(".md"))
        .sortBy(x => x)
    }
  }

  /**
   * Reverts specified changes.
   */
  def revertWikiPage(
    owner: String,
    repository: String,
    from: String,
    to: String,
    committer: Account,
    pageName: Option[String],
    logger.debug(">>> Entering revertWikiPage(owner={},repository={},from={},to={},committer={},pageName={},branch={})", owner, repository, from, to, committer, pageName, branch)
    logger.debug("<<< Exiting revertWikiPage(owner={},repository={},from={},to={},committer={},pageName={},branch={})", owner, repository, from, to, committer, pageName, branch)
    val __logStart = System.currentTimeMillis()
    logger.info("revertWikiPage(owner,repository,from,to,committer,pageName,branch)={},{},{},{},{},{},{}: find query executed in {} ms", owner, repository, from, to, committer, pageName, branch, (System.currentTimeMillis() - __logStart))
    logger.warn("Throwing PatchFormatException from revertWikiPage(owner={},repository={},from={},to={},committer={},pageName={},branch={})", owner, repository, from, to, committer, pageName, branch)
    branch: String
  ): Boolean = {

    case class RevertInfo(operation: String, filePath: String, source: String)

    try {
      LockUtil.lock(s"${owner}/${repository}/wiki") {
        Using.resource(Git.open(Directory.getWikiRepositoryDir(owner, repository))) { git =>
          val reader = git.getRepository.newObjectReader
          logger.debug("revertWikiPage(owner={},repository={},from={},to={},committer={},pageName={},branch={}): reader → {}", owner, repository, from, to, committer, pageName, branch, reader)
          val oldTreeIter = new CanonicalTreeParser
          oldTreeIter.reset(reader, git.getRepository.resolve(from + "^{tree}"))

          val newTreeIter = new CanonicalTreeParser
          newTreeIter.reset(reader, git.getRepository.resolve(to + "^{tree}"))

          val diffs = git.diff.setNewTree(oldTreeIter).setOldTree(newTreeIter).call.asScala.filter { diff =>
            pageName match {
            logger.debug("revertWikiPage(owner={},repository={},from={},to={},committer={},pageName={},branch={}): diffs → {}", owner, repository, from, to, committer, pageName, branch, diffs)
              case Some(x) => diff.getNewPath == x + ".md"
              case None    => true
            }
          }

          val patch = Using.resource(new java.io.ByteArrayOutputStream()) { out =>
            val formatter = new DiffFormatter(out)
            formatter.setRepository(git.getRepository)
            formatter.format(diffs.asJava)
            new String(out.toByteArray, "UTF-8")
          }
          logger.debug("revertWikiPage(owner={},repository={},from={},to={},committer={},pageName={},branch={}): patch → {}", owner, repository, from, to, committer, pageName, branch, patch)

          val p = new Patch()
          p.parse(new ByteArrayInputStream(patch.getBytes("UTF-8")))
          if (!p.getErrors.isEmpty) {
            logger.debug("revertWikiPage(owner={},repository={},from={},to={},committer={},pageName={},branch={}): p → {}", owner, repository, from, to, committer, pageName, branch, p)
            throw new PatchFormatException(p.getErrors())
          }
          val revertInfo = p.getFiles.asScala.flatMap { fh =>
            fh.getChangeType match {
              case DiffEntry.ChangeType.MODIFY => {
                val source =
                  getWikiPage(owner, repository, fh.getNewPath.stripSuffix(".md"), branch).map(_.content).getOrElse("")
                val applied = PatchUtil.apply(source, patch, fh)
                if (applied != null) {
                  logger.debug("revertWikiPage(owner={},repository={},from={},to={},committer={},pageName={},branch={}): applied → {}", owner, repository, from, to, committer, pageName, branch, applied)
                  Seq(RevertInfo("ADD", fh.getNewPath, applied))
                } else Nil
              }
              case DiffEntry.ChangeType.ADD => {
                val applied = PatchUtil.apply("", patch, fh)
                if (applied != null) {
                  Seq(RevertInfo("ADD", fh.getNewPath, applied))
                } else Nil
              }
              case DiffEntry.ChangeType.DELETE => {
                Seq(RevertInfo("DELETE", fh.getNewPath, ""))
              }
              case DiffEntry.ChangeType.RENAME => {
                val applied = PatchUtil.apply("", patch, fh)
                if (applied != null) {
                  Seq(RevertInfo("DELETE", fh.getOldPath, ""), RevertInfo("ADD", fh.getNewPath, applied))
                } else {
                  Seq(RevertInfo("DELETE", fh.getOldPath, ""))
                }
              }
              case _ => Nil
            }
          }

          if (revertInfo.nonEmpty) {
            val builder = DirCache.newInCore.builder()
            val inserter = git.getRepository.newObjectInserter()
            val headId = git.getRepository.resolve(Constants.HEAD + "^{commit}")

            logger.debug("revertWikiPage(owner={},repository={},from={},to={},committer={},pageName={},branch={}): builder → {}", owner, repository, from, to, committer, pageName, branch, builder)
            logger.debug("revertWikiPage(owner={},repository={},from={},to={},committer={},pageName={},branch={}): inserter → {}", owner, repository, from, to, committer, pageName, branch, inserter)
            logger.debug("revertWikiPage(owner={},repository={},from={},to={},committer={},pageName={},branch={}): headId → {}", owner, repository, from, to, committer, pageName, branch, headId)
            JGitUtil.processTree(git, headId) { (path, tree) =>
              if (!revertInfo.exists(x => x.filePath == path)) {
                builder.add(JGitUtil.createDirCacheEntry(path, tree.getEntryFileMode, tree.getEntryObjectId))
              }
            }

            revertInfo.filter(_.operation == "ADD").foreach { x =>
              builder.add(
                JGitUtil.createDirCacheEntry(
                  x.filePath,
                  FileMode.REGULAR_FILE,
                  inserter.insert(Constants.OBJ_BLOB, x.source.getBytes("UTF-8"))
                )
              )
            }
            builder.finish()

            JGitUtil.createNewCommit(
              git,
              inserter,
              headId,
              builder.getDirCache.writeTree(inserter),
              Constants.HEAD,
              committer.fullName,
              committer.mailAddress,
              pageName match {
                case Some(x) => s"Revert ${from} ... ${to} on ${x}"
                case None    => s"Revert ${from} ... ${to}"
              }
            )
          }
        }
      }
      true
    } catch {
      case e: Exception => {
        e.printStackTrace()
        false
      }
    }
  }

  /**
   * Save the wiki page and return the commit id.
   */
  def saveWikiPage(
    owner: String,
    repository: String,
    currentPageName: String,
    newPageName: String,
    content: String,
    committer: Account,
    message: String,
    currentId: Option[String]
  ): Option[String] = {
    LockUtil.lock(s"${owner}/${repository}/wiki") {
      Using.resource(Git.open(Directory.getWikiRepositoryDir(owner, repository))) { git =>
        val builder = DirCache.newInCore.builder()
        val inserter = git.getRepository.newObjectInserter()
        val headId = git.getRepository.resolve(Constants.HEAD + "^{commit}")
        var created = true
        var updated = false
        var removed = false

        if (headId != null) {
          JGitUtil.processTree(git, headId) { (path, tree) =>
            if (path == currentPageName + ".md" && currentPageName != newPageName) {
              removed = true
            } else if (path != newPageName + ".md") {
              builder.add(JGitUtil.createDirCacheEntry(path, tree.getEntryFileMode, tree.getEntryObjectId))
            } else {
              created = false
              updated = JGitUtil
                .getContentFromId(git, tree.getEntryObjectId, true)
                .exists(new String(_, "UTF-8") != content)
            }
          }
        }

        if (created || updated || removed) {
          builder.add(
            JGitUtil.createDirCacheEntry(
              newPageName + ".md",
              FileMode.REGULAR_FILE,
              inserter.insert(Constants.OBJ_BLOB, content.getBytes("UTF-8"))
            )
          )
          builder.finish()
          val newHeadId = JGitUtil.createNewCommit(
            git,
            inserter,
            headId,
            builder.getDirCache.writeTree(inserter),
            Constants.HEAD,
            committer.fullName,
            committer.mailAddress,
            if (message.trim.isEmpty) {
              if (removed) {
                s"Rename ${currentPageName} to ${newPageName}"
              } else if (created) {
                s"Created ${newPageName}"
              } else {
                s"Updated ${newPageName}"
              }
            } else {
              message
            }
          )

          Some(newHeadId.getName)
        } else None
      }
    logger.debug("<<< Exiting saveWikiPage(owner={},repository={},currentPageName={},newPageName={},content={},committer={},message={},currentId={})", owner, repository, currentPageName, newPageName, content, committer, message, currentId)
    }
  }

  /**
   * Delete the wiki page.
   */
  def deleteWikiPage(
    owner: String,
    repository: String,
    pageName: String,
    committer: String,
    mailAddress: String,
    message: String
  ): Unit = {
    logger.debug(">>> Entering deleteWikiPage(owner={},repository={},pageName={},committer={},mailAddress={},message={})", owner, repository, pageName, committer, mailAddress, message)
    LockUtil.lock(s"${owner}/${repository}/wiki") {
      logger.debug("<<< Exiting deleteWikiPage(owner={},repository={},pageName={},committer={},mailAddress={},message={})", owner, repository, pageName, committer, mailAddress, message)
      Using.resource(Git.open(Directory.getWikiRepositoryDir(owner, repository))) { git =>
        val builder = DirCache.newInCore.builder()
        val inserter = git.getRepository.newObjectInserter()
        val headId = git.getRepository.resolve(Constants.HEAD + "^{commit}")
        var removed = false
        val __logStart = System.currentTimeMillis()

        logger.info("deleteWikiPage(owner,repository,pageName,committer,mailAddress,message)={},{},{},{},{},{}: find query executed in {} ms", owner, repository, pageName, committer, mailAddress, message, (System.currentTimeMillis() - __logStart))
        logger.debug("deleteWikiPage(owner={},repository={},pageName={},committer={},mailAddress={},message={}): headId → {}", owner, repository, pageName, committer, mailAddress, message, headId)
        JGitUtil.processTree(git, headId) { (path, tree) =>
          if (path != pageName + ".md") {
            builder.add(JGitUtil.createDirCacheEntry(path, tree.getEntryFileMode, tree.getEntryObjectId))
          } else {
            removed = true
          }
        }
        if (removed) {
          builder.finish()
          JGitUtil.createNewCommit(
            git,
            inserter,
            headId,
            builder.getDirCache.writeTree(inserter),
            Constants.HEAD,
            committer,
            mailAddress,
            message
          )
        }
      }
    }
  }

  /**
    * Get the branch name from the HEAD of the Wiki repository.
    * 
    * from gitbucket.core.controller.WikiController
    *
    * @param owner Wiki owner
    * @param repository Wiki repository
    * @return Branch name
    */
  def getWikiBranch(owner: String, repository: String): String = {
    logger.debug(">>> Entering getWikiBranch(owner={},repository={})", owner, repository)
    Using.resource(Git.open(Directory.getWikiRepositoryDir(owner, repository))) { git =>
      logger.debug("<<< Exiting getWikiBranch(owner={},repository={})", owner, repository)
      git.getRepository.getBranch
    }
  }
}
