import java.io._
import java.net._
import javax.xml.ws.http.HTTPException

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.json.JSON
import scala.util.parsing.json.JSON._

trait Logger {
  val VERBOSE = true

  def yap(msg: => String) = if (VERBOSE) println(msg)
}

trait JsonHelpers {
  def asMap(obj:Any):Option[Map[String, Any]] = {
    obj match {
      case m:Map[String, Any] => Some(m)
      case Some(m:Map[String, Any]) => Some(m)
      case _ => None
    }
  }
  
  def asList(obj:Any):Option[List[Any]] = {
    obj match {
      case a:List[Any] => Some(a)
      case Some(a:List[Any]) => Some(a)
      case _ => None
    }
  }
  
  def asDouble(obj:Any):Option[Double] = {
    try {
      obj match {
        case d:Double => Some(d)
        case Some(d:Double) => Some(d)
        case Some(a:Any) => Some(a.toString.trim.toDouble)
        case _ => Some(obj.toString.trim.toDouble)
      }
    } catch {
      case _ => {
        None
      }
    }
  }

  // poor man's xpath
  def extract(map:Map[String, Any], path:String*):Option[Any] = {
    var ret:Any = map
    for (p <- path) {
      val m = asMap(ret)
      if (!m.isDefined) {
        return None
      }

      m.get.get(p) match {
        case Some(thing:Any) => ret = thing
        case _ => return None
      }
    }

    Some(ret)
  }
}

trait OorbyPokerClient {
  val pokerClient:OorbyPoker

  val botName:String
  val devKey:String

  trait OorbyPoker {
    def take_action(action: String):Map[String, Any]
    def get_next_event:Map[String, Any]
  }
}

trait RemotePokerClient extends OorbyPokerClient with Logger {
  val GAME_CREATOR_HOST = "mcp.oorby.com"
  var CURRENT_ENDPOINT_HOST = "http://" + GAME_CREATOR_HOST

  val pokerClient = new OorbyPoker with JsonHelpers {
    var last_results = ""
    var last_event_id = "NONE"
    val enc = "UTF-8"
  
    def take_action(action: String):Map[String, Any] = {
      try {
        val results = endpoint_post("/v1/poker/bots/" + botName + "/next_event", HashMap(Pair("action", action)))
    
        parseResults(results)
      } catch {
        case ex:HTTPException => if (ex.getStatusCode >= 400 && ex.getStatusCode < 500) {
          return get_next_event
        } else {
          throw ex
        }
      }
    }
  
    def get_next_event:Map[String, Any] = {
      val results = endpoint_get("/v1/poker/bots/" + botName + "/next_event")

      parseResults(results)
    }


    def http_get(url: String): String = {
      yap("")
      yap("GET('" + url + "')")
      yap("---")

      makeReq(() => {
        val u = new URL(url)
        val conn = u.openConnection().asInstanceOf[HttpURLConnection]
        conn.setReadTimeout(0) // no timeout
        conn.setInstanceFollowRedirects(true)
        readResponse(conn)
      })
    }
  
    def http_post(url: String, data: String): String = {
      yap("")
      yap("POST('" + url + "', '" + data + "')")
      yap("---")

      makeReq(() => {
        val u = new URL(url)
        val conn = u.openConnection().asInstanceOf[HttpURLConnection]
        conn.setDoOutput(true)
        conn.setReadTimeout(0) // no timeout
        conn.setInstanceFollowRedirects(true)

        val wr = new OutputStreamWriter(conn.getOutputStream())
        wr.write(data)
        wr.flush
        wr.close()

        readResponse(conn)
      })
    }

    private def makeReq(requestGen:() => (Int, String)):String = {
      while (true) {
        val (code, data) = requestGen()
        if (code == 200) {
          return data
        } else if (code >= 500) {
          yap("got error from server, waiting 10 seconds before retrying " + code)
          Thread.sleep(10)
        } else if (code >= 400) {
          throw new HTTPException(code)
        } else {
          yap("got empty response from server " + code)
        }
      }

      return ""
    }

    def readResponse(conn:HttpURLConnection):(Int, String) = {
      yap(conn.getResponseCode() + " " + conn.getResponseMessage())
      val respCode = conn.getResponseCode()
      val istr = if (conn.getResponseCode() < 300) { conn.getInputStream() } else { conn.getErrorStream() }
      val in = new BufferedReader(new InputStreamReader(istr))
      var line = new String
      var stop = false
    
      var response = ""
    
      while (!stop) {
        val line = in.readLine()
        if (line == null)
          stop = true
        else {
          response += line + "\n"
          yap(line)
        }
      }
    
      in.close()
    
      yap("---")
      yap("")
    
      (respCode, response)
    }

    def endpoint_post(resource: String, parameters: HashMap[String, String]): String = {
      val url = CURRENT_ENDPOINT_HOST + resource + "?devkey=" + devKey + "&eventId=" + last_event_id
      val data = parameters.map(kv => URLEncoder.encode(kv._1, enc) + "=" + URLEncoder.encode(kv._2, enc)).mkString("&")

      http_post(url, data)
    }
  
    def endpoint_get(resource: String): String = {
      val url = CURRENT_ENDPOINT_HOST + resource + "?devkey=" + devKey + "&eventId=" + last_event_id
    
      http_get(url)
    }

    def parseResults(results: String):Map[String, Any] = {
      last_results = results
    
      for {
        resultsJson <- JSON.parseFull(results)
        resultsMap <- asMap(resultsJson)
        event <- asMap(resultsMap.get("event"))
        game <- asMap(event.get("game"))
        eventId <- resultsMap.get("eventId")
      } {
        last_event_id = eventId.toString
        return resultsMap      
      }
      Map()
    }
  }
}


trait PokerEventListener {
  def eventReceived(eventMap:Map[String, Any])
}

trait Bot extends Logger with JsonHelpers {
  self: OorbyPokerClient =>

  val eventListeners:List[PokerEventListener]

  def play:Unit = play(-1)

  def play(numGames:Int):Unit = {
    yap("Joining game, may take up to 30 seconds")

    var numGamesPlayed = 0
    var nextAction:Option[String] = None
    while (numGames == -1 || numGames > numGamesPlayed) {
      val event = if (nextAction.isDefined) pokerClient.take_action(nextAction.get) else pokerClient.get_next_event

      eventListeners.map(_.eventReceived(event))

      val eventType = extract(event, "event", "eventType")
      if (eventType.exists(_ == "ActionRequired")) {
        nextAction = Some(decideNextAction(event))
      } else {
        nextAction = None
      }

      if (eventType.exists(_ == "GameComplete")) {
        numGamesPlayed += 1
      }
    }
  }

  def decideNextAction(eventMap:Map[String, Any]):String

}

class EventPrinter(val botName:String) extends PokerEventListener with JsonHelpers {

  def eventReceived(resultsMap:Map[String, Any]) = {
    for {
      event <- asMap(resultsMap.get("event"))
      game <- asMap(event.get("game"))
    } {
      event.get("eventType") match {
        case Some("HandComplete") => {
          for (hand <- asMap(event.get("hand"))) {
            val gameId = game.get("gameId").getOrElse("?")
            printCompletedHand(gameId.toString, hand)
          }
        }
        
        case Some("GameComplete") => {
          println("(This game is now complete.)\n")
          for (hand <- asMap(event.get("lastHand"))) {
            val gameId = game.get("gameId").getOrElse("?")
            printCompletedHand(gameId.toString, hand)
          }
        }
        
        case Some("ActionRequired") => {
          for {
            stakes <- game.get("playerStakes")
            stakesArray <- asList(stakes)
            hand <- asMap(event.get("hand"))
          } {
            println("Current stakes:")
            
            for {
              stake <- stakesArray
              stakeMap <- asMap(stake)
              stakeBotName <- stakeMap.get("botName")
              currentStake <- stakeMap.get("currentStake")
            } {
              val me = if (botName == stakeBotName) " (me)" else ""
              println("  " + stakeBotName + " -> " + currentStake + me)
            }
            println
            
            asMap(hand.get("communityCards")).map(c => printCards("Cards on the table:", c.get("cards")))
            asMap(hand.get("hole")).map(c => printCards("Your cards:", c.get("cards")))
            
            printAvailableActions(hand)
          }
        }
        
        case _ => {} // event type we don't care about
      }
    }
  }
  
  private def printCards(msg:String, cardsOpt:Option[Any]) = {
    for {
      cards <- asList(cardsOpt)
      if (cards.length > 0)
    } {
      println(msg)
      print_hand(cards.map(_.toString))
      println()
    }
  }
  
  val actionLabels = Map("f" -> "fold", "r" -> "raise", "c" -> "call")
  private def printAvailableActions(hand:Map[String, Any]) = {
    for {
      availableActions <- asList(hand.get("availableActions"))
      availableAction <- availableActions
      actionMap <- asMap(availableAction)
      actionKey <- actionMap.get("action")
      actionCost <- asDouble(actionMap.get("costOfAction"))
    } {
      val costString = if (actionCost > 0) " for " + actionCost else ""
      println("Press " + actionKey + " to " + actionLabels(actionKey.toString) + costString)
    }
  }
  
  private def printCompletedHand(gameId:String, hand:Map[String, Any]) = {
    val handNumber = hand.get("handNumber").getOrElse("?")
    println("\n\n\n\n\n*** Game " + gameId + ", hand " + handNumber + "\n")
    
    println("(This hand is now complete.)\n")
    val showdownHoles = asList(hand.get("showdownPlayerHoles"))
    if (showdownHoles.isDefined) {
      printShowdown(showdownHoles.get)
    }
    
    println("Results:")
    var myChipChange = 0.0
    for {
      results <- asList(hand.get("results"))
      result <- results
      resultsMap <- asMap(result)
      botName <- resultsMap.get("botName")
      netChipChange <- resultsMap.get("netStackChange")
    } {
      if (botName.equals(botName)) {
        myChipChange = netChipChange.toString.toDouble
      } else {
        println("\tbot " + botName + " -> " + netChipChange)
      }
    }    
    println("\tMe -> " + myChipChange + "\n")
  }
  
  private def printShowdown(showdown:List[Any]) = {
    if (showdown.nonEmpty) {
      println("Showdown:")
      
      for {
        hole <- showdown
        holeMap <- asMap(hole)
      } {
        val resultBotName = holeMap("botName")
        val bot = if (resultBotName.equals(botName)) {
          "Your"
        } else {
          "bot " + resultBotName
        }
        
        asMap(holeMap.get("hole")).map(cards => printCards("  " + bot + " hole:", cards.get("cards")))
        asMap(holeMap.get("bestHand")).map(cards => printCards("  " + bot + " best hand:", cards.get("cards")))
      }
    }
  }

  def print_hand(cards: List[String]) = {
    for (row <- 0 to 5) {
      for (card <- cards)
        print(card_line(card, row) + " ")
      
      println
    }
  }
  
  // return a single line of a card rendering
  def card_line(card: String, line: Int): String = {
    val rank = card.charAt(0)
    val suit = card.charAt(1)
    
    // Should probably validate that rank is 2-9TJQKA and suit is hcsd
    
    var rank_left = ""
    var rank_right = ""
    
    if (rank == 'T') {
      rank_left = "10"
      rank_right = "10"
    } else {
      if (suit == 'h') {
        rank_left = rank + "_"
      } else {
        rank_left = rank + " "
      }
      rank_right = " " + rank
    }
    
    val art = Map(
      'h' -> List(".------.", "|" + rank_left + "  _ |", "|( \\/ )|", "| \\  / |", "|  \\/" + rank_right + "|", "`------'"),
      'd' -> List(".------.", "|" + rank_left + "/\\  |", "| /  \\ |", "| \\  / |", "|  \\/" + rank_right + "|", "`------'"),
      'c' -> List(".------.", "|" + rank_left + "_   |", "| ( )  |", "|(_x_) |", "|  Y " + rank_right + "|", "`------'"),
      's' -> List(".------.", "|" + rank_left + ".   |", "| / \\  |", "|(_,_) |", "|  I " + rank_right + "|", "`------'")
    )
    
    return art(suit)(line)
  }
}


class CallBot(val botName:String, val devKey:String) extends Bot
                   with RemotePokerClient {
  val eventListeners = List(new EventPrinter(botName))
  def decideNextAction(eventMap:Map[String, Any]):String = {
    "c"
  }
}

class RandomBot(val botName:String, val devKey:String) extends Bot
                   with RemotePokerClient {
  val eventListeners = List(new EventPrinter(botName))
  def decideNextAction(eventMap:Map[String, Any]):String = {
    import scala.util.Random._
    val availableActions = extract(eventMap, "event", "hand", "availableActions")

    for {
      availableActions <- asList(extract(eventMap, "event", "hand", "availableActions"))
      randomAction <- asMap(availableActions(nextInt(availableActions.length)))
    } {
      return randomAction.get("action").get.toString
    }
    "c"
  }
}


object BotRunner {
  def main(args: Array[String]) {
    if (args.size != 2) {
      println("usage: scala BotRunner botName devKey")
      return
    }

    val bot = new RandomBot(args(0), args(1))
    bot.play(5)
  }
}
