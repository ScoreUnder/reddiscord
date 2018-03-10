package score.discord.redditbot

import java.io.{File, PrintWriter}

import score.discord.redditbot.DiscordUtil._
import score.discord.redditbot.ResourceManagement.autoClose

class PinStorage {
  type PinMap = Map[ChanID, Seq[MesgID]]

  private[this] var pins: PinMap = Map.empty

  def getPinsFromFile(file: File): PinMap = {
    var newPins: PinMap = Map.empty
    for (line <- io.Source.fromFile(file).getLines) {
      val Array(chan, mesgs@_*) = line.split(" ").map(parseID)
      newPins += chan -> mesgs
    }
    newPins
  }

  def loadPins(file: File): Unit = {
    pins = getPinsFromFile(file)
  }

  def savePins(file: File): Unit = {
    autoClose(new PrintWriter(file)) { file =>
      for ((chan, mesg) <- pins) {
        file.println(s"$chan ${mesg mkString " "}")
      }
    }
  }

  def update(chanID: ChanID, msgs: Seq[MesgID]): Unit = {
    pins += chanID -> msgs.toVector
  }

  def apply(chanID: ChanID): Option[Seq[MesgID]] = pins.get(chanID)
}
