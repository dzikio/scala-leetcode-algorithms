package io.dzik.leetcode

import scala.collection.mutable
import scala.util.{Try, Using}

object ProblemsParser {
  def fromPath(path: String): Try[List[ProblemSummary]] = {
    Using(scala.io.Source.fromFile(path)) { src =>
      ProblemsParser(src.getLines())
    }
  }

  def apply(content: Iterator[String]): List[ProblemSummary] = {
    def getBlocks(): List[String] = {
      val res = mutable.ArrayBuffer.empty[String]
      var isBlock = false
      val sb = new StringBuilder()

      for (line <- content) {
        if (isBlock) {
          if (line.contains("*/")) {
            res += sb.toString()
            sb.clear()
            isBlock = false
          } else {
            val startI = line.indexOf("*") + 1
            val ss = line.substring(startI).trim

            if (ss.nonEmpty) {
              sb += '\n'
              sb.append(ss)
            }
          }
        } else if (!isBlock && line.contains("/**")) {
          isBlock = true
          val startI = line.indexOf("/**") + 3
          val ss = line.substring(startI).trim

          if (ss.nonEmpty) {
            sb.append(ss)
          }
        }
      }

      res.toList
    }

    def parseBlock(block: String): ProblemSummary = {
      val lines = block.split('\n')
      var i = 0
      var title: String = null
      var difficulty: String = null
      var link: String = null

      while (i < lines.length) {
        val line = lines(i)
        if (line.contains("Title:")) {
          val trimmed = line.dropWhile(_ != ':').drop(1).trim
          if (trimmed.nonEmpty) {
            title = trimmed
            i += 1
          } else {
            title = lines(i + 1).trim
            i += 2
          }
        } else if (line.contains("Difficulty:")) {
          val trimmed = line.dropWhile(_ != ':').drop(1).trim
          if (trimmed.nonEmpty) {
            difficulty = trimmed
            i += 1
          } else {
            difficulty = lines(i + 1).trim
            i += 2
          }
        } else if (line.contains("Link:")) {
          val trimmed = line.dropWhile(_ != ':').drop(1).trim
          if (trimmed.nonEmpty) {
            link = trimmed
            i += 1
          } else {
            link = lines(i + 1).trim
            i += 2
          }
        } else {
          i += 1
        }
      }

      ProblemSummary(title, difficulty, link)
    }

    getBlocks().map(parseBlock)
  }

}
