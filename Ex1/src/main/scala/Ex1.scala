import akka.actor._
import akka.routing.RoundRobinRouter

sealed trait Message

case object Start extends Message
case class Ping(nome: Int) extends Message
case class Pong(nome: Int) extends Message

class AtorA(n: Int, private[this] var actB: ActorRef) extends Actor{
    
    def receive: Receive = {
        case Start => {
            actB ! Ping(0)
        }
        
        case Pong(n) => {
            if(n == 2000){
                println("Acabou")
                context.stop(self)
            }else{
                println("Pong" + (n+1))
                sender() ! Ping(n+1)
            }
        }
    }
}

class AtorB(n: Int) extends Actor{
    
    def receive: Receive = {
        case Ping(n) => {
            if(n == 2000){
                println("Acabou")
                context.stop(self)
            }else{
                println("Ping" + (n+1))
                sender() ! Pong(n+1)
            }
        }
    }
}

object TesteEx1{
    def main (args: Array[String]): Unit = {
        val sys = ActorSystem("MainSystem")
        val atorB = sys.actorOf(Props(new AtorB(0)), "atorB")
        val atorA = sys.actorOf(Props(new AtorA(0, atorB)), "atorA")
        
        atorA ! Start
    }
}