import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._

sealed trait Message

case object Calculate extends Message

case class Work(start:Int, number: Int) extends Message
case class Result(valor: Double) extends Message
case class Show(conta: Double, duration: Duration) extends Message

class Worker extends Actor{
    
    def conta(start : Int, number: Int): Double = {
        var soma = 0.0
        soma = number - start
        
        return soma
    }
    
    override def receive: Receive = {
        case Work(start,fim) => sender() ! Result(conta(start,fim))
    }
    
}

class Master(var numFat: Int, listener: ActorRef) extends Actor{
    var conta: Double = 1
    var resultados: Int = _
    val start: Long = System.currentTimeMillis // COMECO
    
    val worker = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(numFat)), "worker")
    
    override def receive: Receive = {
        
        if(numFat <= 1){
            case Calculate => {
                listener ! Show(conta, (System.currentTimeMillis - start).millis)
                context.stop(self)
            }
        }
    
        else
        {
            case Calculate => {
                for(i<- 0 until numFat - 1)
                    worker ! Work(i,numFat)
            }
                        
            case Result(res) => {
                conta *= res 
                resultados += 1
                    
                if(resultados == numFat - 1){
                    listener ! Show(conta, (System.currentTimeMillis - start).millis)
                    context.stop(self)
                }
            }
        }
    }
}

class Listener extends Actor{
    def receive: Receive = {
      case Show(res, duration) ⇒
        println("Resultado: \t\t%s\n\tTempo: \t%s".format(res, duration))
    }
}

object TesteEx4{
    def main(args: Array[String]): Unit = {
        val system = ActorSystem("MainSystem")
 
        val listener = system.actorOf(Props[Listener], "listener")
        val masterActor = system.actorOf(Props(new Master(5, listener)),"masterActor")
 
        masterActor ! Calculate
    }
}