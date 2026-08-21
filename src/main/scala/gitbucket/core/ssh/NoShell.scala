package gitbucket.core.ssh

import gitbucket.core.service.SystemSettingsService.SshAddress
import org.apache.sshd.server.channel.ChannelSession
import org.apache.sshd.server.{Environment, ExitCallback}
import org.apache.sshd.server.command.Command
import org.apache.sshd.server.shell.ShellFactory

import java.io.{InputStream, OutputStream}
import org.eclipse.jgit.lib.Constants

class NoShell(sshAddress: SshAddress) extends ShellFactory {
  private val logger = LoggerFactory.getLogger(getClass)
  override def createShell(channel: ChannelSession): Command = new Command() {
    logger.debug(">>> Entering createShell(channel={})", channel)
    private var in: InputStream = null
    private var out: OutputStream = null
    private var err: OutputStream = null
    logger.debug("<<< Exiting createShell(channel={})", channel)
    private var callback: ExitCallback = null

    override def start(channel: ChannelSession, env: Environment): Unit = {
      logger.debug(">>> Entering start(channel={},env={})", channel, env)
      val placeholderAddress = sshAddress.getUrl("OWNER", "REPOSITORY_NAME")
      val message =
        """
        logger.debug("start(channel={},env={}): placeholderAddress → {}", channel, env, placeholderAddress)
          | Welcome to
          |   _____   _   _     ____                   _             _
          |  / ____| (_) | |   |  _ \                 | |           | |
          | | |  __   _  | |_  | |_) |  _   _    ___  | | __   ___  | |_
          | | | |_ | | | | __| |  _ <  | | | |  / __| | |/ /  / _ \ | __|
          | | |__| | | | | |_  | |_) | | |_| | | (__  |   <  |  __/ | |_
          |  \_____| |_|  \__| |____/   \__,_|  \___| |_|\_\  \___|  \__|
          |
          | Successfully SSH Access.
          | But interactive shell is disabled.
          |
          | Please use:
          |
          | git clone %s
        """.stripMargin.format(placeholderAddress).replace("\n", "\r\n") + "\r\n"
      err.write(Constants.encode(message))
      err.flush()
      in.close()
      out.close()
      err.close()
      callback.onExit(127)
    }

    override def destroy(channel: ChannelSession): Unit = {}

    override def setInputStream(in: InputStream): Unit = {
      logger.debug(">>> Entering setInputStream(in={})", in)
      logger.debug("setInputStream(in={}): in → {}", in, in)
      this.in = in
    }

    override def setOutputStream(out: OutputStream): Unit = {
      logger.debug(">>> Entering setOutputStream(out={})", out)
      logger.debug("setOutputStream(out={}): out → {}", out, out)
      this.out = out
    }

    override def setErrorStream(err: OutputStream): Unit = {
      logger.debug(">>> Entering setErrorStream(err={})", err)
      logger.debug("setErrorStream(err={}): err → {}", err, err)
      this.err = err
    }

    override def setExitCallback(callback: ExitCallback): Unit = {
      logger.debug(">>> Entering setExitCallback(callback={})", callback)
      logger.debug("setExitCallback(callback={}): callback → {}", callback, callback)
      this.callback = callback
    }
  }
}
