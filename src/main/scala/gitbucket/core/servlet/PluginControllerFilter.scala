package gitbucket.core.servlet

import javax.servlet._

import gitbucket.core.controller.ControllerBase
import gitbucket.core.plugin.PluginRegistry

class PluginControllerFilter extends ControllerFilter {
  private val logger = LoggerFactory.getLogger(getClass)

  private var filterConfig: FilterConfig = null

  override def init(filterConfig: FilterConfig): Unit = {
    logger.debug(">>> Entering init(filterConfig={})", filterConfig)
    logger.debug("init(filterConfig={}): filterConfig → {}", filterConfig, filterConfig)
    this.filterConfig = filterConfig
  }

  override def destroy(): Unit = {
    logger.debug(">>> Entering destroy()")
    PluginRegistry().getControllers().foreach { case (controller, _) =>
      controller.destroy()
    logger.debug("<<< Exiting destroy()")
    }
  }

  override def process(request: ServletRequest, response: ServletResponse, checkPath: String): Boolean = {
    logger.debug(">>> Entering process(request={},response={},checkPath={})", request, response, checkPath)
    PluginRegistry()
      .getControllers()
      .filter { case (_, path) =>
        val start = path.replaceFirst("/\\*$", "/")
        checkPath.startsWith(start)
      }
      .foreach { case (controller, _) =>
        controller match {
          case x: ControllerBase if (x.config == null) => x.init(filterConfig)
          case _                                       => ()
        }
        val mockChain = new MockFilterChain()
        controller.doFilter(request, response, mockChain)

        logger.debug("process(request={},response={},checkPath={}): mockChain → {}", request, response, checkPath, mockChain)
        if (mockChain.continue == false) {
          return false
        }
      logger.debug("<<< Exiting process(request={},response={},checkPath={})", request, response, checkPath)
      }

    true
  }

}
