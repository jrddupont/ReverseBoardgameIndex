package com.github.jrddupont

import com.typesafe.config.{Config, ConfigFactory}

import java.io.{File, FileNotFoundException}
import scala.jdk.CollectionConverters._

/**
 * I usually make a class like this in every project I work on to help reading in config files
 */
object ConfigUtil {

	// ConfigFactory.parseFile does not throw an error if the file is not found
	// So I manually do that to prevent confusion
	def safeReadConfig(path: String): Config ={
		val file = new File(path)
		if( file.exists() ){
			ConfigFactory.parseFile(file)
		} else {
			throw new FileNotFoundException("File not found: " + path)
		}
	}

	case class IndexConfig(
		automatic: Boolean,
		automaticGames: List[String],
		playersSource: String,
		manualConfig: List[(String, List[Int])]
	)

	// I don't like passing around config objects, so I typically parse them out into structs
	def parseGameConfig(config: Config): IndexConfig = {
		val games = config.getConfigList("manual.games").asScala.toList
		val manualConfig = games.map(gameConfig => {
			val name = gameConfig.getString("name")
			val players = gameConfig.getIntList("players").asScala.toList.map(_.toInt)
			(name, players)
		})
		val isAutomatic = config.getString("index.source") match {
			case "automatic" => true
			case "manual" => false
			case default => throw new Exception("Unrecognized index.source value: " + default)
		}

		IndexConfig(
			isAutomatic,
			config.getStringList("automatic.games").asScala.toList,
			config.getString("automatic.players").toLowerCase(),
			manualConfig
		)
	}
}