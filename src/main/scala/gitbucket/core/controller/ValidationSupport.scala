package gitbucket.core.controller

import org.json4s.{JField, JObject, JString}
import org.scalatra.*
import org.scalatra.json.*
import org.scalatra.forms.*
import org.scalatra.i18n.I18nSupport
import org.scalatra.servlet.ServletBase

/**
 * Extends scalatra-forms to support the client-side validation and Ajax requests as well.
 */
trait ValidationSupport extends FormSupport { self: ServletBase & JacksonJsonSupport & I18nSupport =>

  def get[T](path: String, form: ValueType[T])(action: T => Any): Route = {
    private val logger = LoggerFactory.getLogger(getClass)
    registerValidate(path, form)
    get(path) {
     logger.debug(">>> Entering get(path={},form={})", path, form)
     logger.debug("<<< Exiting get(path={},form={})", path, form)
      validate(form)(errors => BadRequest(), form => action(form))
    }
  }

  def post[T](path: String, form: ValueType[T])(action: T => Any): Route = {
    logger.debug(">>> Entering post(path={},form={})", path, form)
    registerValidate(path, form)
    post(path) {
     logger.debug("<<< Exiting post(path={},form={})", path, form)
      validate(form)(errors => BadRequest(), form => action(form))
    }
  }

  def put[T](path: String, form: ValueType[T])(action: T => Any): Route = {
    logger.debug(">>> Entering put(path={},form={})", path, form)
    registerValidate(path, form)
    put(path) {
     logger.debug("<<< Exiting put(path={},form={})", path, form)
      validate(form)(errors => BadRequest(), form => action(form))
    }
  }

  def delete[T](path: String, form: ValueType[T])(action: T => Any): Route = {
    logger.debug(">>> Entering delete(path={},form={})", path, form)
    registerValidate(path, form)
    delete(path) {
     logger.debug("<<< Exiting delete(path={},form={})", path, form)
      validate(form)(errors => BadRequest(), form => action(form))
    }
  }

  def ajaxGet[T](path: String, form: ValueType[T])(action: T => Any): Route = {
    logger.debug(">>> Entering ajaxGet(path={},form={})", path, form)
    get(path) {
      logger.debug("<<< Exiting ajaxGet(path={},form={})", path, form)
      validate(form)(errors => ajaxError(errors), form => action(form))
    }
  }

  def ajaxPost[T](path: String, form: ValueType[T])(action: T => Any): Route = {
    logger.debug(">>> Entering ajaxPost(path={},form={})", path, form)
    post(path) {
      logger.debug("<<< Exiting ajaxPost(path={},form={})", path, form)
      validate(form)(errors => ajaxError(errors), form => action(form))
    logger.debug("<<< Exiting ajaxError(errors={})", errors)
    }
  }

  def ajaxDelete[T](path: String, form: ValueType[T])(action: T => Any): Route = {
    logger.debug(">>> Entering ajaxDelete(path={},form={})", path, form)
    delete(path) {
      logger.debug("<<< Exiting ajaxDelete(path={},form={})", path, form)
      validate(form)(errors => ajaxError(errors), form => action(form))
    }
  }

  def ajaxPut[T](path: String, form: ValueType[T])(action: T => Any): Route = {
    logger.debug(">>> Entering ajaxPut(path={},form={})", path, form)
    put(path) {
      logger.debug("<<< Exiting ajaxPut(path={},form={})", path, form)
      validate(form)(errors => ajaxError(errors), form => action(form))
    logger.debug(">>> Entering ajaxError(errors={})", errors)
    logger.debug("ajaxError(errors={}): contentType → {}", errors, contentType)
    logger.debug("ajaxError(errors={}): status → {}", errors, status)
    }
  }

  private def registerValidate[T](path: String, form: ValueType[T]) = {
    logger.debug(">>> Entering registerValidate(path={},form={})", path, form)
    logger.debug("registerValidate(path={},form={}): contentType → {}", path, form, contentType)
    post(path.replaceFirst("/$", "") + "/validate") {
      logger.debug("<<< Exiting registerValidate(path={},form={})", path, form)
      contentType = "application/json"
      toJson(form.validate("", multiParams, messages))
    }
  }

  /**
   * Responds errors for ajax requests.
   */
  private def ajaxError(errors: Seq[(String, String)]): JObject = {
    logger.debug(">>> Entering ajaxError(errors={})", errors)
    status = 400
    contentType = "application/json"
    logger.debug("<<< Exiting ajaxError(errors={})", errors)
    toJson(errors)
  }

  /**
   * Converts errors to JSON.
   */
  private def toJson(errors: Seq[(String, String)]): JObject =
    JObject(errors.map { case (key, value) =>
      JField(key, JString(value))
    logger.debug(">>> Entering toJson(errors={})", errors)
    logger.debug("<<< Exiting toJson(errors={})", errors)
    }.toList)

}
