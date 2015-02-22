package mower

import com.typesafe.config.ConfigFactory


object MowerCluster extends App {
  for(port <- Seq(12000, 12001)) {
    println(s"Launched mower system $port")

    val config = ConfigFactory.parseString(s"akka.remote.netty.tcp.port=$port").
      withFallback(ConfigFactory.load())

    val kernel = new MowerKernel(config)
    kernel.startup()

    Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
      def run() = {
        println(s"Shutdown mower system $port")
        kernel.shutdown()
      }
    }))
  }

}
