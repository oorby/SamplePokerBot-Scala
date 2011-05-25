import java.io._
import java.net._

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.json.JSON
import scala.util.parsing.json.JSON._

object SampleBot
{
  val ENDPOINT_HOST = "localhost:8080";
  val VERBOSE = false
  
  var ARG_BOT_NAME = "";
  var ARG_DEV_KEY = "";
  
  var last_results = "";
  var last_event_id = "NONE";
  val enc = "UTF-8";
  
  def yap(msg: String): Unit = {
    if (VERBOSE)
      println(msg)
  }
  
  def http_get(url: String): String = {
    yap("")
    yap("GET('" + url + "')")
    yap("---")
    
    val u = new URL(url);
    val conn = u.openConnection().asInstanceOf[HttpURLConnection];
    conn.setReadTimeout(0) // no timeout
    conn.setInstanceFollowRedirects(true)
    
    println(conn.getResponseCode() + " " + conn.getResponseMessage())
    val istr = if (conn.getResponseCode() == 200) { conn.getInputStream() } else { conn.getErrorStream() }
    val in = new BufferedReader(new InputStreamReader(istr))
    var line = new String
    var stop = false
    
    var response = ""
    
    while (!stop) {
      val line = in.readLine()
      if (line == null)
        stop = true
      else {
        yap(line)
        response += line + "\n"
      }
    }
    
    in.close()
    yap("---")
    yap("")
    
    response
  }
  
  def http_post(url: String, data: String): String = {
    yap("")
    yap("POST('" + url + "', '" + data + "')")
    yap("---")
    
    val u = new URL(url);
    val conn = u.openConnection().asInstanceOf[HttpURLConnection];
    conn.setDoOutput(true)
    conn.setReadTimeout(0) // no timeout
    conn.setInstanceFollowRedirects(true)

    val wr = new OutputStreamWriter(conn.getOutputStream())
    wr.write(data)
    wr.flush
    
    println(conn.getResponseCode() + " " + conn.getResponseMessage())
    val istr = if (conn.getResponseCode() == 200) { conn.getInputStream() } else { conn.getErrorStream() }
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
    wr.close()
    
    yap("---");
    yap("");
    
    response
  }
  
  def endpoint_post(resource: String, parameters: HashMap[String, String]): String = {
    val url = "http://" + ENDPOINT_HOST + resource + "?devkey=" + ARG_DEV_KEY + "&eventId=" + last_event_id
    
    var data = ""
    
    var first = true
    for ((k, v) <- parameters) {
      if (!first)
        data += "&"
      data += URLEncoder.encode(k, enc) + "=" + URLEncoder.encode(v, enc)
      first = false
    }
    
    http_post(url, data)
  }
  
  def endpoint_get(resource: String): String = {
    val url = "http://" + ENDPOINT_HOST + resource + "?devkey=" + ARG_DEV_KEY + "&eventId=" + last_event_id
    
    http_get(url)
  }
  
  def take_action(action: String):Map[String, Any] = {
    val results = endpoint_post("/v1/poker/bots/" + ARG_BOT_NAME + "/next_event", HashMap(Pair("action", action)))
    
    print_results(results)
  }
  
  def get_next_event:Map[String, Any] = {
    val results = endpoint_get("/v1/poker/bots/" + ARG_BOT_NAME + "/next_event");

    print_results(results)
  }
  
  def print_hand(cards: List[String]) = {
    for (row <- 0 to 5) {
      for (card <- cards)
        print(card_line(card, row) + " ");
      
      println
    }
  }
  
  // return a single line of a card rendering
  def card_line(card: String, line: Int): String = {
    val rank = card.charAt(0)
    val suit = card.charAt(1)
    
    // Should probably validate that rank is 2-9TJQKA and suit is hcsd
    
    var rank_left = "";
    var rank_right = "";
    
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
    );
    
    return art(suit)(line);
  }
  
  def print_help {
    println("c - call");
    println("r - raise");
    println("f - fold");
    println("n - next event");
    println("j - dump JSON from last query");
    println("q - leave the program");
    println
  }
  
  private def asMap(obj:Any):Option[Map[String, Any]] = {
    obj match {
      case m:Map[String, Any] => Some(m)
      case Some(m:Map[String, Any]) => Some(m)
      case _ => None
    }
  }
  
  private def asList(obj:Any):Option[List[Any]] = {
    obj match {
      case a:List[Any] => Some(a)
      case Some(a:List[Any]) => Some(a)
      case _ => None
    }
  }
  
  private def asDouble(obj:Any):Option[Double] = {
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

  def print_results(results: String):Map[String, Any] = {
    last_results = results
    
    for {
      resultsJson <- JSON.parseFull(results)
      resultsMap <- asMap(resultsJson)
      event <- asMap(resultsMap.get("event"))
      game <- asMap(event.get("game"))
      eventId <- resultsMap.get("eventId")
    } {
      last_event_id = eventId.toString
      
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
              botName <- stakeMap.get("botName")
              currentStake <- stakeMap.get("currentStake")
            } {
              val me = if (botName == ARG_BOT_NAME) " (me)" else ""
              println("  " + botName + " -> " + currentStake + me)
            }
            println
            
            asMap(hand.get("communityCards")).map(c => printCards("Cards on the table:", c.get("cards")))
            asMap(hand.get("hole")).map(c => printCards("Your cards:", c.get("cards")))
            
            printAvailableActions(hand)
          }
        }
        
        case _ => {} // event type we don't care about
      }

      return resultsMap      
    }

    Map()
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
      if (botName.equals(ARG_BOT_NAME)) {
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
        val botName = if (resultBotName.equals(ARG_BOT_NAME)) {
          "Your"
        } else {
          "bot " + resultBotName
        }
        
        val bothole = asMap(holeMap.get("hole"))
        if (bothole.isDefined) {
          printCards("  " + botName + " hole:", bothole.get.get("cards"))
        }
        
        val besthand = asMap(holeMap.get("bestHand"))
        if (besthand.isDefined) {
          printCards("  " + botName + " best hand:", besthand.get.get("cards"))
        }
      }
    }
  }

  def main(args: Array[String]) {
    if (args.size != 2) {
      println("usage: scala SampleBot botName guid")
      return
    }
    
    ARG_BOT_NAME = args(0)
    ARG_DEV_KEY = args(1)
    
    // So we don't forget why we're here...
    print_hand(List("Th", "Js", "Qc", "Kd", "Ac"))
    
    println("Joining game, may take up to 30 seconds")

    // interactiveLoop()

    var nextAction:Option[String] = None
    while (true) {
      val eventMap = if (nextAction.isDefined) {
		    println("Submitting action " + nextAction.get)
		    take_action(nextAction.get)
      } else {
        get_next_event
      }

      val eventType = extract(eventMap, "event", "eventType")
      if (eventType.exists(_.equals("ActionRequired"))) {
        nextAction = Some(decideNextAction(eventMap))
      } else {
        nextAction = None
      }
    }
  }


  // CHANGE THIS METHOD WITH YOUR OWN POKER LOGIC
  def decideNextAction(eventMap:Map[String, Any]):String = {
    import scala.util.Random._

    for {
      availableActions <- asList(extract(eventMap, "event", "hand", "availableActions"))
      randomAction <- asMap(availableActions(nextInt(availableActions.length)))
    } {
      return randomAction.get("action").get.toString
    }
    "c"
  }


  def interactiveLoop() = {
    get_next_event
    var quit = false    

    do {
      print(": ")
      val l = readLine()
        
      if (l.length > 0)
        l.charAt(0) match {
          case 'c' => take_action("c")
          case 'r' => take_action("r")
          case 'f' => take_action("f")
          case 'n' => get_next_event
          case 'j' => println(last_results)
          case 'h' => print_help
          case 'q' => quit = true
          case _ => print_help
        }
      else print_results(last_results)
    } while (!quit)
  }
}
