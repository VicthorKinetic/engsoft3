import akka.actor._
import akka.routing.RoundRobinRouter

sealed trait Message

case object Start extends Message
case object Guess extends Message
case class Result(valor: Int) extends Message
case class Show(certo: Int) extends Message

class Guesser extends Actor{
    def guess: Int = {
        val r = new scala.util.Random
        val n = 1 + r.nextInt((200 - 1) + 1);

        return n
    }
    
    override def receive: Receive = {
        case Guess => sender() ! Result(guess)
    }

}

class Master(num: Int, listener: ActorRef) extends Actor{
    var correct = 0
    var resultados: Int = _
    val guesser = context.actorOf(Props[Guesser].withRouter(RoundRobinRouter(32)), "guesser")
    
    override def receive: Receive = {
        case Start => {
            for(i<- 0 until 32)
                guesser ! Guess
        }
        
        case Result(guess) => {
            resultados+= 1
            println(guess)
            
            if(guess == num){
                println("O jogador " + resultados + " acertou")
                correct+= 1
            }
            
            if(resultados == 32){
                listener ! Show(correct)
            }
        }
    }
}

class Listener extends Actor{
    def receive: Receive = {
      case Show(certo) ⇒
      if(certo == 0)
      {
          println("Nenhum acertou")
      }
      else
      {
          if(certo > 1)
          {
              println(certo + " jogadores acertaram o número")
          }
          else
          {
              println(certo + " jogador acertou o número")
          }
      }
    }
}

object TesteEx2{
    def main (args: Array[String]): Unit = {
        val system = ActorSystem("MainSystem")
        
        val listener = system.actorOf(Props[Listener],"listener")
        val masterActor = system.actorOf(Props(new Master (200, listener)),"masterActor")
    
        
        masterActor ! Start
    }
}
