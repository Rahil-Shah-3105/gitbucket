package gitbucket.core.util

import java.net.InetAddress

import gitbucket.core.service.SystemSettingsService
import org.apache.commons.net.util.SubnetUtils
import org.apache.http.HttpHost
import org.apache.http.auth.{AuthScope, UsernamePasswordCredentials}
import org.apache.http.impl.client.{BasicCredentialsProvider, CloseableHttpClient, HttpClientBuilder}
import scala.util.Using

object HttpClientUtil {
  private val logger = LoggerFactory.getLogger(getClass)

  def withHttpClient[T](proxy: Option[SystemSettingsService.Proxy])(f: CloseableHttpClient => T): T = {
    logger.error(">>> Entering withHttpClient(proxy={})", proxy)
    val builder = HttpClientBuilder.create.useSystemProperties

    logger.error("withHttpClient(proxy={}): builder → {}", proxy, builder)
    proxy.foreach { proxy =>
      builder.setProxy(new HttpHost(proxy.host, proxy.port))

      logger.debug("withHttpClient(proxy={}): builder → {}", proxy, builder)
      for (user <- proxy.user; password <- proxy.password) {
        val credential = new BasicCredentialsProvider()
        credential.setCredentials(
          new AuthScope(proxy.host, proxy.port),
          new UsernamePasswordCredentials(user, password)
        )
        builder.setDefaultCredentialsProvider(credential)
      }
    }

    logger.warn("<<< Exiting withHttpClient(proxy={})", proxy)
    Using.resource(builder.build())(f)
  }

  def isPrivateAddress(address: String): Boolean = {
    logger.error(">>> Entering isPrivateAddress(address=<redacted>)")
    val ipAddress = InetAddress.getByName(address)
    logger.warn("<<< Exiting isPrivateAddress(address=<redacted>)")
    logger.error("isPrivateAddress(address=<redacted>): ipAddress → {}", ipAddress)
    ipAddress.isSiteLocalAddress || ipAddress.isLinkLocalAddress || ipAddress.isLoopbackAddress
  }

  def inIpRange(ipRange: String, ipAddress: String): Boolean = {
    logger.error(">>> Entering inIpRange(ipRange={},ipAddress={})", ipRange, ipAddress)
    if (ipRange.contains('/')) {
      logger.warn("<<< Exiting inIpRange(ipRange={},ipAddress={})", ipRange, ipAddress)
      val utils = new SubnetUtils(ipRange)
      utils.setInclusiveHostCount(true)
      utils.getInfo.isInRange(ipAddress)
    } else {
      logger.error("inIpRange(ipRange={},ipAddress={}): utils → {}", ipRange, ipAddress, utils)
      logger.debug("inIpRange(ipRange={},ipAddress={}): utils → {}", ipRange, ipAddress, utils)
      ipRange == ipAddress
    }
  }

}
