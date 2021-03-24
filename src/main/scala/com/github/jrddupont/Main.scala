package com.github.jrddupont

import com.github.jrddupont.ConfigUtil.IndexConfig
import com.jayway.jsonpath.JsonPath.using
import com.jayway.jsonpath.{Configuration, JsonPath}
import com.jayway.jsonpath.Option
import net.minidev.json.JSONArray

import scala.io.Source
import scala.jdk.CollectionConverters._

object Main {
	val configPath = "resources/games.conf"
	def main(args: Array[String]): Unit = {
		// Read in config
		val config = ConfigUtil.safeReadConfig(configPath)
		val indexConfig = ConfigUtil.parseGameConfig(config)

		// Determine what source to pull the player number list from
		// either from the config file or from boardgamegeek
		val gamesList = if(indexConfig.automatic){
			// Get the player list from the internet
			val gmeList = getGamesListFromInternet(indexConfig)
			printConfig(gmeList)
			gmeList
		} else {
			indexConfig.manualConfig
		}

		// Print the index
		printReverseIndex(gamesList)
	}

	// This function goes to each url and parses the json there to get the list of player counts
	def getGamesListFromInternet(indexConfig: IndexConfig):  List[(String, List[Int])] = {
		// For each URL
		indexConfig.automaticGames.map(gameURL => {
			// Read the HTML from the website
			val source = Source.fromURL(gameURL)

			// Extract the json from the page
			val unfilteredJSONString = source.getLines()
			  .find(_.contains("GEEK.geekitemPreload"))
			val rawJSONString = unfilteredJSONString match {
				case None => throw new Exception("No Json found")
				case Some(value) => value
			}
			val jsonString = rawJSONString
			  .replaceFirst("\tGEEK.geekitemPreload = ", "")
			  .init

			// Parse the json using JayWay Json parser
			val jsonDocument = Configuration.defaultConfiguration().jsonProvider().parse(jsonString)

			// Get the name of the game
			val rawName = JsonPath.read(jsonDocument, "$.item.name").toString

			// Get the absolute max and min players from the root
			val hardMin = JsonPath.read(jsonDocument, "$.item.minplayers").toString.toInt
			val hardMax = JsonPath.read(jsonDocument, "$.item.maxplayers").toString.toInt

			// Depending on the configured source, get players from the hardMin/Max or get it from the user poll section
			val rawPlayers = indexConfig.playersSource match {
				case "supported" =>  (hardMin to hardMax).toList
				case "recommended" => getPollsRange(jsonDocument, "recommended", hardMin, hardMax)
				case "best" => getPollsRange(jsonDocument, "best", hardMin, hardMax)
			}

			// Special logic for games
			val players = if(rawName == "Codenames"){
				rawPlayers.filter(_ % 2 == 0)
			} else {
				rawPlayers
			}

			// Making names smaller so they are easier to read
			val name = rawName match {
				case "Epic Spell Wars of the Battle Wizards: Duel at Mt. Skullzfyre" => "Epic Spell Wars"
				case "Betrayal at House on the Hill" => "Betrayal"
				case default => default
			}

			source.close()
			(name, players)
		})
	}

	// In the recommended and best section of the JSON there can be multiple ranges of recommended players
	// eg 2-4, 6-8
	// So we parse them all into integer ranges
	val conf: Configuration = Configuration.builder().options(Option.AS_PATH_LIST).build();
	def getPollsRange(jsonDocument: AnyRef, source: String, hardMin: Int, hardMax: Int): List[Int] = {
		// Get a list of the paths to the multiple ranges
		val jsonArray = using(conf).parse(jsonDocument).read("$.item.polls.userplayers." + source + ".*"): JSONArray
		val pathList = jsonArray.asScala.toList

		// For each path, read the ranges from the json
		val allPlayers = pathList.flatMap(jsonPath => {
			val rawMin = JsonPath.read(jsonDocument, jsonPath + ".min"): Int
			val rawMax = JsonPath.read(jsonDocument, jsonPath + ".max"): Int
			// Just some basic sanity checks
			val min = if(rawMin == 0 || rawMin < hardMin){
				hardMin
			} else{
				rawMin
			}
			val max = if(rawMax == 0 || rawMax > hardMax){
				hardMax
			} else{
				rawMax
			}
			(min to max).toList
		})
		val players = allPlayers.sorted.distinct
		players
	}

	// Helper function to print out the config of a player count list
	def printConfig(gamesList: List[(String, List[Int])]): Unit = {
		println("manual.games: [")
		gamesList.foreach(game => {
			val name = game._1
			val players = game._2.mkString(", ")
			val string = s"""  { name: "$name", players: [$players] },"""
			println(string)
		})
		println("]")
	}

	// Perform the reverse index and print it out
	def printReverseIndex(gamesList: List[(String, List[Int])]): Unit = {

		// Flatten the game list from (game -> [2, 3, 4]) into (game -> 2, game -> 3, game -> 4)
		val flatGamesList = gamesList.flatMap(game => {
			game._2.map(players => {
				(game._1, players)
			})
		})

		// Group all the entries by their player count ex (2 -> ["game1", "game2", "game3"])
		// Reduce those groups into a string (2 -> "[game1, game2, game3]")
		// Sort by player count
		// transform into one large string, eg "2 -> [game1, game2, game3]"
		// And finally print the output
		flatGamesList
		  .groupBy(_._2)
		  .map(pairs => {
			  val number = pairs._1
			  val games = pairs._2.map(_._1).mkString("[", ", ", "]")
			  (number, games)
		  }).toList
		  .sortWith(_._1 < _._1)
		  .map(tuple => tuple._1 + " -> " + tuple._2)
		  .foreach(println)
	}
}
