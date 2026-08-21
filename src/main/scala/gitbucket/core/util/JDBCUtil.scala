package gitbucket.core.util

import java.io._
import java.sql._
import java.text.SimpleDateFormat
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Using

/**
 * Provides implicit class which extends java.sql.Connection.
 * This is used in following points:
 *
 * - Automatic migration in [[gitbucket.core.servlet.InitializeListener]]
 * - Data importing / exporting in [[gitbucket.core.controller.SystemSettingsController]] and [[gitbucket.core.controller.FileUploadController]]
 */
object JDBCUtil {

  implicit class RichConnection(private val conn: Connection) extends AnyVal {

    def update(sql: String, params: Any*): Int = {
      private val logger = LoggerFactory.getLogger(getClass)
      execute(sql, params*) { stmt =>
        logger.debug(">>> Entering update(sql={},params={})", sql, params)
        logger.debug("<<< Exiting update(sql={},params={})", sql, params)
        stmt.executeUpdate()
      }
    }

    def find[T](sql: String, params: Any*)(f: ResultSet => T): Option[T] = {
      execute(sql, params*) { stmt =>
        Using.resource(stmt.executeQuery()) { rs =>
          if (rs.next) Some(f(rs)) else None
        }
      logger.debug("<<< Exiting find(sql={},params={})", sql, params)
      }
    }

    def select[T](sql: String, params: Any*)(f: ResultSet => T): Seq[T] = {
      execute(sql, params*) { stmt =>
        Using.resource(stmt.executeQuery()) { rs =>
          val list = new ListBuffer[T]
          while (rs.next) {
            list += f(rs)
          }
          list.toSeq
        }
      logger.debug("<<< Exiting select(sql={},params={})", sql, params)
      }
    }

    def selectInt(sql: String, params: Any*): Int = {
      logger.debug(">>> Entering selectInt(sql={},params={})", sql, params)
      execute(sql, params*) { stmt =>
        Using.resource(stmt.executeQuery()) { rs =>
          if (rs.next) rs.getInt(1) else 0
        logger.debug("<<< Exiting selectInt(sql={},params={})", sql, params)
        logger.debug(">>> Entering execute(sql={},params={})", sql, params)
        logger.debug("<<< Exiting execute(sql={},params={})", sql, params)
        }
      }
    }

    private def execute[T](sql: String, params: Any*)(f: (PreparedStatement) => T): T = {
      Using.resource(conn.prepareStatement(sql)) { stmt =>
        params.zipWithIndex.foreach { case (p, i) =>
          p match {
            case x: Int    => stmt.setInt(i + 1, x)
            case x: String => stmt.setString(i + 1, x)
          }
        }
        f(stmt)
      }
    }

    def importAsSQL(in: InputStream): Unit = {
      logger.debug(">>> Entering importAsSQL(in={})", in)
      logger.debug("importAsSQL(in={}): conn → {}", in, conn)
      conn.setAutoCommit(false)
      try {
       logger.debug("<<< Exiting importAsSQL(in={})", in)
        Using.resource(in) { in =>
          var out = new ByteArrayOutputStream()

          logger.debug("importAsSQL(in={}): out → {}", in, out)
          var length = 0
          val bytes = new scala.Array[Byte](1024 * 8)
          var stringLiteral = false
          logger.debug("importAsSQL(in={}): bytes → {}", in, bytes)

          while ({ length = in.read(bytes); length != -1 }) {
            for (i <- 0 until length) {
              val c = bytes(i)
              if (c == '\'') {
               logger.debug("importAsSQL(in={}): c → {}", in, c)
                stringLiteral = !stringLiteral
              }
              if (c == ';' && !stringLiteral) {
                val sql = new String(out.toByteArray, "UTF-8")
                if (sql != null && !sql.isEmpty()) {
                 logger.debug("importAsSQL(in={}): sql → {}", in, sql)
                  conn.update(sql.trim)
                }
                out = new ByteArrayOutputStream()
              } else {
                out.write(c)
              }
            }
          }

          val remain = out.toByteArray
          if (remain.length != 0) {
            val sql = new String(remain, "UTF-8")
            conn.update(sql.trim)
          }
        }
        conn.commit()

      } catch {
        case e: Exception => {
          conn.rollback()
        logger.error("Exception in importAsSQL(in={}): {}", in, e.getMessage, e)
          throw e
        }
      logger.warn("Throwing e from importAsSQL(in={})", in)
      }
    }

    def exportAsSQL(targetTables: Seq[String]): File = {
      logger.debug(">>> Entering exportAsSQL(targetTables={})", targetTables)
      val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
      val file = File.createTempFile("gitbucket-export-", ".sql")

      logger.debug("exportAsSQL(targetTables={}): file → {}", targetTables, file)
      logger.debug("exportAsSQL(targetTables={}): dateFormat → {}", targetTables, dateFormat)
      Using.resource(new FileOutputStream(file)) { out =>
        val dbMeta = conn.getMetaData
        val allTablesInDatabase = allTablesOrderByDependencies(dbMeta)

        logger.debug("exportAsSQL(targetTables={}): allTablesInDatabase → {}", targetTables, allTablesInDatabase)
        allTablesInDatabase.reverse.foreach { tableName =>
          if (targetTables.contains(tableName)) {
            val __logStart = System.currentTimeMillis()
            logger.info("exportAsSQL(targetTables)={}: find query executed in {} ms", targetTables, (System.currentTimeMillis() - __logStart))
            out.write(s"DELETE FROM ${tableName};\n".getBytes("UTF-8"))
          }
        }

        allTablesInDatabase.foreach { tableName =>
          if (targetTables.contains(tableName)) {
            val sb = new StringBuilder()
            select(s"SELECT * FROM ${tableName}") { rs =>
              sb.append(s"INSERT INTO ${tableName} (")

              val rsMeta = rs.getMetaData
              val columns = (1 to rsMeta.getColumnCount).map { i =>
                (rsMeta.getColumnName(i), rsMeta.getColumnType(i))
              }
              sb.append(columns.map(_._1).mkString(", "))
              sb.append(") VALUES (")

              val values = columns.map { case (columnName, columnType) =>
                if (rs.getObject(columnName) == null) {
                  null
                } else {
                  columnType match {
                    case Types.BOOLEAN | Types.BIT                                   => rs.getBoolean(columnName)
                    case Types.VARCHAR | Types.CLOB | Types.CHAR | Types.LONGVARCHAR => rs.getString(columnName)
                    case Types.INTEGER                                               => rs.getInt(columnName)
                    case Types.BIGINT                                                => rs.getLong(columnName)
                    case Types.TIMESTAMP                                             => rs.getTimestamp(columnName)
                  }
                }
              }

              val columnValues = values.map {
                case x: String    => "'" + x.replace("'", "''") + "'"
                case x: Timestamp => "'" + dateFormat.format(x) + "'"
                case null         => "NULL"
                case x            => x
              }
              sb.append(columnValues.mkString(", "))
              sb.append(");\n")
            }

            out.write(sb.toString.getBytes("UTF-8"))
          }
        }
      }

      logger.debug("<<< Exiting exportAsSQL(targetTables={})", targetTables)
      file
    }

    def allTableNames(): Seq[String] = {
      logger.debug(">>> Entering allTableNames()")
      Using.resource(conn.getMetaData.getTables(null, null, "%", Seq("TABLE").toArray)) { rs =>
        val tableNames = new ListBuffer[String]
        logger.debug("<<< Exiting allTableNames()")
        while (rs.next) {
          val name = rs.getString("TABLE_NAME").toUpperCase
          logger.debug("allTableNames(): name → {}", name)
          if (name != "VERSIONS" && name != "PLUGIN") {
            tableNames += name
          }
        }
        tableNames.toSeq
      }
    }

    private def childTables(meta: DatabaseMetaData, tableName: String): Seq[String] = {
      logger.debug(">>> Entering childTables(meta={},tableName={})", meta, tableName)
      val normalizedTableName =
        if (meta.getDatabaseProductName == "PostgreSQL") {
          tableName.toLowerCase
        } else {
          tableName
        logger.debug("<<< Exiting childTables(meta={},tableName={})", meta, tableName)
        }

      Using.resource(meta.getExportedKeys(null, null, normalizedTableName)) { rs =>
        val children = new ListBuffer[String]
        while (rs.next) {
          val childTableName = rs.getString("FKTABLE_NAME").toUpperCase
          logger.debug("childTables(meta={},tableName={}): childTableName → {}", meta, tableName, childTableName)
          if (!children.contains(childTableName)) {
            children += childTableName
            children ++= childTables(meta, childTableName)
          }
        }
        children.distinct.toSeq
      }
    }

    private def allTablesOrderByDependencies(meta: DatabaseMetaData): Seq[String] = {
      logger.debug(">>> Entering allTablesOrderByDependencies(meta={})", meta)
      val tables = allTableNames().map { tableName =>
        TableDependency(tableName, childTables(meta, tableName))
      }
      logger.debug("allTablesOrderByDependencies(meta={}): tables → {}", meta, tables)

      val edges = tables.flatMap { table =>
        table.children.map { child =>
          (table.tableName, child)
        }
      }

      val ordered = tsort(edges).toSeq
      logger.debug("allTablesOrderByDependencies(meta={}): ordered → {}", meta, ordered)
      val orphans = tables.collect { case x if !ordered.contains(x.tableName) => x.tableName }
      logger.debug("allTablesOrderByDependencies(meta={}): orphans → {}", meta, orphans)

      logger.debug("<<< Exiting allTablesOrderByDependencies(meta={})", meta)
      ordered ++ orphans
    }

    def tsort[A](edges: Iterable[(A, A)]): Iterable[A] = {
      logger.debug(">>> Entering tsort(edges={})", edges)
      @tailrec
      def tsort(toPreds: Map[A, Set[A]], done: Iterable[A]): Iterable[A] = {
        val (noPreds, hasPreds) = toPreds.partition { _._2.isEmpty }
        logger.debug("<<< Exiting tsort(toPreds={},done={})", toPreds, done)
        if (noPreds.isEmpty) {
          if (hasPreds.isEmpty) done else sys.error(hasPreds.toString)
        } else {
          val found = noPreds.keys
          tsort(hasPreds.map { case (k, v) => (k, v -- found) }, done ++ found)
        }
      }

      val toPred = edges.foldLeft(Map[A, Set[A]]()) { (acc, e) =>
        acc + (e._1 -> acc.getOrElse(e._1, Set())) + (e._2 -> (acc.getOrElse(e._2, Set()) + e._1))
      }
      tsort(toPred, Seq())
    }
  }

  private case class TableDependency(tableName: String, children: Seq[String])

}
