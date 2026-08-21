package gitbucket.core.util

import java.net.InetAddress

import gitbucket.core.service.SystemSettingsService
import org.apache.commons.net.util.SubnetUtils
import org.apache.http.HttpHost
import org.apache.http.auth.{AuthScope, UsernamePasswordCredentials}
import org.apache.http.impl.client.{BasicCredentialsProvider, CloseableHttpClient, HttpClientBuilder}
import scala.util.Using

object HttpClientUtil {

  def withHttpClient[T](proxy: Option[SystemSettingsService.Proxy])(f: CloseableHttpClient => T): T = {
    private val logger = LoggerFactory.getLogger(getClass)
    val builder = HttpClientBuilder.create.useSystemProperties
    logger.debug(">>> Entering withHttpClient(proxy={})", proxy)

    proxy.foreach { proxy =>
      builder.setProxy(new HttpHost(proxy.host, proxy.port))

      for (user <- proxy.user; password <- proxy.password) {
        val credential = new BasicCredentialsProvider()
        credential.setCredentials(
          new AuthScope(proxy.host, proxy.port),
          new UsernamePasswordCredentials(user, password)
        )
        builder.setDefaultCredentialsProvider(credential)
      }
    }

    logger.debug("<<< Exiting withHttpClient(proxy={})", proxy)
    Using.resource(builder.build())(f)
  }

  def isPrivateAddress(address: String): Boolean = {
    logger.debug(">>> Entering isPrivateAddress(address={})", address)
    val ipAddress = InetAddress.getByName(address)
    logger.debug("<<< Exiting isPrivateAddress(address={})", address)
    logger.debug("isPrivateAddress(address={}): ipAddress → {}", address, ipAddress)
    ipAddress.isSiteLocalAddress || ipAddress.isLinkLocalAddress || ipAddress.isLoopbackAddress
  }

  def inIpRange(ipRange: String, ipAddress: String): Boolean = {
    logger.debug(">>> Entering inIpRange(ipRange={},ipAddress={})", ipRange, ipAddress)
    if (ipRange.contains('/')) {
      logger.debug("<<< Exiting inIpRange(ipRange={},ipAddress={})", ipRange, ipAddress)
      val utils = new SubnetUtils(ipRange)
      utils.setInclusiveHostCount(true)
      utils.getInfo.isInRange(ipAddress)
    } else {
      logger.debug("inIpRange(ipRange={},ipAddress={}): utils → {}", ipRange, ipAddress, utils)
      ipRange == ipAddress
    }
  }

}
