package mud

import akka.actor.Actor
import akka.actor.ActorRef

class NPC(name: String) extends Actor {
  private var currentRoom: ActorRef = null
  private var health: Int = 100
  private var currentWeapon: Option[Item] = Some(Item(" ", 30, 20, ""))
  private var currentVictim: Option[ActorRef] = None

  import NPC._
  import Player._

 /* This game is structures so the NPC cannot not initiate or react in combat because it does not have a weapon.
  * NPCs are just mindless people roaming the ship.
  * */
  
  def receive = {
    case TakeExit(newRoom) =>
      newRoom match {
        case Some(nextRoom) =>
          if (currentRoom != null) currentRoom ! Room.PExit(self)
          currentRoom = nextRoom
          currentRoom ! Room.PEnter(self)
        case None => println("Error in room movement for NPC")
      }
      Main.actSuper ! ActivityManager.ScheduleEvent(200, Walk)

    //NPC moves randomly among rooms
    case Walk =>
      currentRoom ! Room.GetExit(util.Random.nextInt(6))
    


    /*Attack*/

    //prepare for attack
    case DoAttack =>
      currentVictim match {
        case Some(x) =>
              x ! Attack(currentWeapon.get.damage, currentRoom)
        case None =>
      }
    //attacking
    case Attack(damage, room) =>
      if (room == currentRoom) {
        val sameRoom = true
        health -= damage
        if (!currentVictim.isEmpty) {
          currentVictim = Some(sender)
          Main.actSuper ! ActivityManager.ScheduleEvent(currentWeapon.get.speed, DoAttack)
        }
        val dead = if (health <= 0) {
          currentRoom ! Room.PExit(self)
          context.stop(self)

          true
        } else false

        sender ! AttackDone(dead, sameRoom)
      } else sender ! AttackDone(false, false)

    //attack complete
    case AttackDone(victimDead, sameRoom) =>
      if (victimDead && sameRoom) {

        currentVictim = None
      } else if (!victimDead && !sameRoom) {

        currentVictim = None
      } else {
        Main.actSuper ! ActivityManager.ScheduleEvent(currentWeapon.get.speed, DoAttack)
      }

    case PrintMessage(msg) =>
    //NPCs don't print messages

    case m => println("unknown message in NPC: " + m)
  }

}

object NPC {
  case object Walk
}