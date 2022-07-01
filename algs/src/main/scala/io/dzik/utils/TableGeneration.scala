package io.dzik.utils

import io.dzik.leetcode.ProblemsParser

object TableGeneration extends App {
  ProblemsParser
    .fromPath(
      "algs/src/main/scala/io/dzik/leetcode/Solution.scala"
    )
    .get
    .foreach(ps => {
      println(s"|${ps.difficulty(0)}|[${ps.title}](${ps.link})|")
    })
}
