package cosc250.roboScala

import java.awt.Color
import akka.actor.Actor
import scala.util.Random

/**
  * Spinning duck turns in circles, while also spinning its turret and radar (making its
  * radar sweep fast). It pings constantly, and if it sees a tank always fires.
  */
class WallPatroler extends Actor {
  
  // Give our tank a unique name
  val name = s"Wall Patroler ${Random.alphanumeric.take(4).mkString}"

  // As soon as the tank is created, register it
  Main.gameActor ! Register(name, Color.orange)
  def receive:PartialFunction[Any, Unit] = {

    // Every game tick, we receive our tank state, and should send commands to
    // Main.gameActor to say what we want to do this tick

    //Note in GameSTate there is an inBounds methods that chaecks if the tank will be in the boundaries
    case TankState(me) =>
      Main.gameActor ! FullSpeedAhead
      if (me.canPing) Main.gameActor ! RadarPing
      
      if (!GameState.inBounds(me.position + (Vec2.fromRTheta(me.velocity,me.facing)))) 
        Main.gameActor ! TurnClockwise 

      
      val ang = math.atan2(GameState.height/2 - me.position.y, GameState.width/2 - me.position.x)

      if (ang >= 0 && me.turretFacing < ang ) then {
      Main.gameActor ! TurretClockwise
      }
      else if (ang < 0 && me.turretFacing < ang) then {
      Main.gameActor ! TurretClockwise
     }
      else {
      Main.gameActor ! TurretAnticlockwise
      }
  

    // If we successfully Ping the radar, we'll get a message containing the
    // states of any tanks we see
    case RadarResult(me, seenTanks) =>
      if (seenTanks.nonEmpty) sender() ! Fire

  }

}
