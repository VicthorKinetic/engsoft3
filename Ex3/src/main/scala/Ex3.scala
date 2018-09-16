import akka.actor._
import akka.routing.RoundRobinRouter

sealed trait Message

case class Start(nome: String) extends Message
case class Mensagem(message: String) extends Message

class DataSource(private[this] var prox: ActorRef) extends Actor{
    
    def receive: Receive = {
        case Start(message) => {
            prox ! Mensagem(message)
        }
    }
}

class LowerCase(private[this] var prox: ActorRef) extends Actor{
    
    def receive: Receive = {
        case Mensagem(message) => {
            println(message.toLowerCase)
            if (prox == null)
            {
                println("Acabou")
            }
            else
            {
                prox ! Mensagem(message.toLowerCase)
            }
        }
    }
}

class UpperCase(private[this] var prox: ActorRef) extends Actor{
    
    def receive: Receive = {
        case Mensagem(message) => {
            println(message.toUpperCase)
            if (prox == null)
            {
                println("Acabou")
            }
            else
            {
                prox ! Mensagem(message.toUpperCase)
            }
        }
    }
}

class FilterVowels(private[this] var prox: ActorRef) extends Actor{
    
    def receive: Receive = {
        case Mensagem(message) => {
            println(message.replaceAll("[aeiouAEIOU]", ""))
            if (prox == null)
            {
                println("Acabou")
            }
            else
            {
                prox ! Mensagem(message.replaceAll("[aeiouAEIOU]", ""))
            }
        }
    }
}

class Duplicate(private[this] var prox: ActorRef) extends Actor{
    
    def receive: Receive = {
        case Mensagem(message) => {
            println(message + " " + message)
            if (prox == null)
            {
                println("Acabou")
            }
            else
            {
                prox ! Mensagem(message + " " + message)
            }
        }
    }
}

object TesteEx3{
    def main (args: Array[String]): Unit = {
        val sys = ActorSystem("MainSystem")
        //val duplicate = sys.actorOf(Props(new Duplicate(null)), "Duplicate")
        //val filtervowels = sys.actorOf(Props(new FilterVowels(duplicate)), "FilterVowels")
        //val uppercase = sys.actorOf(Props(new UpperCase(filtervowels)), "UpperCase")
        //val lowercase = sys.actorOf(Props(new LowerCase(uppercase)), "LowerCase")
        //val datasource = sys.actorOf(Props(new DataSource(lowercase)), "DataSource")
        
        val lowercase = sys.actorOf(Props(new LowerCase(null)), "LowerCase")
        val filtervowels = sys.actorOf(Props(new FilterVowels(lowercase)), "FilterVowels")
        val duplicate = sys.actorOf(Props(new Duplicate(filtervowels)), "Duplicate")
        val uppercase = sys.actorOf(Props(new UpperCase(duplicate)), "UpperCase")
        val datasource = sys.actorOf(Props(new DataSource(uppercase)), "DataSource")
        
        datasource ! Start("Mensagem de teste do programa")
    }
}