package matcher

import com.typesafe.config.ConfigFactory

object Launcher extends App {

  Matcher(ConfigFactory.load().getBoolean("useRegex")).scan()

}
