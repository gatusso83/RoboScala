package cosc250.roboScala

import java.awt.Color
import akka.actor.Actor
import scala.util.Random
import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import akka.pattern.{AskTimeoutException, ask}
import akka.util.Timeout
import scala.collection.mutable._

import scala.concurrent.Future
import scala.concurrent.duration._



/**
  * Spinning duck turns in circles, while also spinning its turret and radar (making its
  * radar sweep fast). It pings constantly, and if it sees a tank always fires.
  */
class MyVeryOwnTank extends Actor {
  import context.dispatcher
  implicit val timeout:Timeout = 5.seconds

  val log = Logging(context.system, this)

  //Patrol points
  //var positionList:Queue[Vec2] = Queue(Vec2(20,20),Vec2(GameState.width - 20, 20), Vec2(20, GameState.height -20),Vec2(GameState.width - 20,GameState.height -20),
                 //                   Vec2(20, GameState.height -20), Vec2(GameState.width - 20, 20))
  
  var positionList:Queue[Vec2] = Queue(Vec2(GameState.width - 20,GameState.height -20),Vec2(20, GameState.height -20),Vec2(GameState.width - 20, 20),Vec2(20,20),
                                            Vec2(20, GameState.height -20),Vec2(GameState.width - 20, 20))

  // Give our tank a unique name
  val name = s"MyVeryOwnTank ${Random.alphanumeric.take(4).mkString}"

  // As soon as the tank is created, register it
  Main.gameActor ! Register(name, Color.orange)
  def receive:PartialFunction[Any, Unit] = {
    
    // Every game tick, we receive our tank state, and should send commands to
    // Main.gameActor to say what we want to do this tick

    case TankState(me) =>
      
      //println(me.facing)
      if (me.canPing) Main.gameActor ! RadarPing

      var ang = math.atan2(positionList.front._2 - me.position.y , positionList.front._1 - me.position.x )
      if (ang < 0)then
        ang += 2*Math.PI // Issue is now with this angle when looking for 2nd position
      println("Ang: "+ang)
      println("Tank facing: "+ me.facing )

      
      if (( me.position.y >= positionList.front._2 -20 && me.position.y <= positionList.front._2 + 20) && 
            (me.position.x >= positionList.front._1 -20 && me.position.x <= positionList.front._1 + 20 ) ) {
        val deQ = positionList.dequeue
        positionList.enqueue(deQ)
        
      }
      else if (me.facing <= ang - 0.1) then
          Main.gameActor ! TurnClockwise
      else if me.facing > (ang + 0.1) then
          Main.gameActor ! TurnAnticlockwise
      else 
        Main.gameActor ! FullSpeedAhead
        
          

      

      //if (me.position._1 =! positionList._1._1 && me.position._2 != positionList._1._1)


      /*//Top Left corner
      if ((me.position._1 < 70  && me.position._2 < 70) && me.facing != Vec2.E) then{//(me.facing <= Vec2.E-0.005 || me.facing >= Vec2.E+0.005)) then {
        Main.gameActor ! TurnClockwise
      }
      
      //Bottom Right Corner
      if ((me.position._1 > GameState.width - 50  && me.position._2 > GameState.height - 50) && (me.facing <= Vec2.W-0.005 || me.facing >= Vec2.W+0.005)) then {
        Main.gameActor ! TurnClockwise

      }
      //Bottom Left Corner
      if ((me.position._1 < 70  && me.position._2 > GameState.height - 70) && me.facing != Vec2.NE) then {//(me.facing <= Vec2.NE || me.facing >= Vec2.NE+0.005)) then {
        Main.gameActor ! TurnClockwise
      }

      //Top Right corner
      if ((me.position._1 < GameState.width - 70 && me.position._2 < 70) && (me.facing >= Vec2.NW && me.facing <= Vec2.NE+0.005)) then {
        Main.gameActor ! TurnAnticlockwise
      }
      //if ((me.position._1 > GameState.width - 100 && me.position._2 < 100) && (me.facing <= Vec2.NE-0.1 || me.facing >= Vec2.NE+0.1))

      if (GameState.inBounds(me.position + (Vec2.fromRTheta(me.velocity,me.facing)))) Main.gameActor ! FullSpeedAhead 
        if (!GameState.inBounds(me.position + (Vec2.fromRTheta(me.velocity,me.facing))) && me.position._2 < GameState.height/2) Main.gameActor ! TurnAnticlockwise
        else if (!GameState.inBounds(me.position + (Vec2.fromRTheta(me.velocity,me.facing))) && me.position._2 > GameState.height/2) Main.gameActor ! TurnClockwise
      */
 
  

    // If we successfully Ping the radar, we'll get a message containing the
    // states of any tanks we see
    case RadarResult(me, seenTanks) =>
      if (seenTanks.nonEmpty) sender() ! Fire

    case Insulted(insult) =>
      
      val sentBy = sender()
      log.info("The insult was - {}", insult)
      for {
        retort <- Main.insultsActor ? WhatsTheRetortFor(insult)}
        yield { 

        log.info("The retort send back to {} was - {}", sentBy, retort)
        sentBy ! retort}

      



  }

}
