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
		val config = ConfigUtil.safeReadConfig(configPath)
		val indexConfig = ConfigUtil.parseGameConfig(config)

		val gamesList = if(indexConfig.automatic){
			val gmeList = getGamesListFromInternet(indexConfig)
			printConfig(gmeList)
			gmeList
		} else {
			indexConfig.manualConfig
		}

		printReverseIndex(gamesList)
	}

	def getGamesListFromInternet(indexConfig: IndexConfig):  List[(String, List[Int])] = {
		indexConfig.automaticGames.map(gameURL => {
			val source = Source.fromURL(gameURL)
			val unfilteredJSONString = source.getLines()
			  .find(_.contains("GEEK.geekitemPreload"))
			val rawJSONString = unfilteredJSONString match {
				case None => throw new Exception("No Json found")
				case Some(value) => value
			}
			val jsonString = rawJSONString
			  .replaceFirst("\tGEEK.geekitemPreload = ", "")
			  .init

			val jsonDocument = Configuration.defaultConfiguration().jsonProvider().parse(jsonString)

			val rawName = JsonPath.read(jsonDocument, "$.item.name").toString

			val hardMin = JsonPath.read(jsonDocument, "$.item.minplayers").toString.toInt
			val hardMax = JsonPath.read(jsonDocument, "$.item.maxplayers").toString.toInt

			val rawPlayers = indexConfig.playersSource match {
				case "supported" =>  (hardMin to hardMax).toList
				case "recommended" => getPollsRange(jsonDocument, "recommended", hardMin, hardMax)
				case "best" => getPollsRange(jsonDocument, "best", hardMin, hardMax)
			}

			val players = if(rawName == "Codenames"){
				rawPlayers.filter(_ % 2 == 0)
			} else {
				rawPlayers
			}

			val name = rawName match {
				case "Epic Spell Wars of the Battle Wizards: Duel at Mt. Skullzfyre" => "Epic Spell Wars"
				case "Betrayal at House on the Hill" => "Betrayal"
				case default => default
			}

			source.close()
			(name, players)
		})
	}
	val conf: Configuration = Configuration.builder().options(Option.AS_PATH_LIST).build();
	def getPollsRange(jsonDocument: AnyRef, source: String, hardMin: Int, hardMax: Int): List[Int] = {
		val jsonArray = using(conf).parse(jsonDocument).read("$.item.polls.userplayers." + source + ".*"): JSONArray
		val pathList = jsonArray.asScala.toList
		val allPlayers = pathList.flatMap(jsonPath => {
			val rawMin = JsonPath.read(jsonDocument, jsonPath + ".min"): Int
			val rawMax = JsonPath.read(jsonDocument, jsonPath + ".max"): Int
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

	def printReverseIndex(gamesList: List[(String, List[Int])]): Unit = {
		val flatGamesList = gamesList.flatMap(game => {
			game._2.map(players => {
				(game._1, players)
			})
		})

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
