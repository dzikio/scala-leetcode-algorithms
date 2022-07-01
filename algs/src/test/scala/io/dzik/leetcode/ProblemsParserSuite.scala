package io.dzik.leetcode

class ProblemsParserSuite extends munit.FunSuite {
  test("It works!") {}

  val NormalCase = """  /** Title: 100. Short title
                     |    *
                     |    * Difficulty: Medium
                     |    *
                     |    * Link: https://normallink.com
                     |    */""".stripMargin.split('\n')

  test("Normal case") {
    assertEquals(
      ProblemsParser(NormalCase.iterator),
      List(
        ProblemSummary("100. Short title", "Medium", "https://normallink.com")
      )
    )
  }

  val LinkInNewLine = """  /** Title: 200. Short title
                        |    *
                        |    * Difficulty: Medium
                        |    *
                        |    * Link:
                        |    * https://longlink.com
                        |    */""".stripMargin.split('\n')
  test("LinkInNewLine case") {
    assertEquals(
      ProblemsParser(LinkInNewLine.iterator),
      List(
        ProblemSummary("200. Short title", "Medium", "https://longlink.com")
      )
    )
  }

  val TitleInNewLine = """ /** Title: 
                        |    * 300. Very long title
                        |    *
                        |    * Difficulty: Medium
                        |    *
                        |    * Link: https://normallink.com
                        |    */""".stripMargin.split('\n')
  test("TitleInNewLine case") {
    assertEquals(
      ProblemsParser(TitleInNewLine.iterator),
      List(
        ProblemSummary(
          "300. Very long title",
          "Medium",
          "https://normallink.com"
        )
      )
    )
  }

  test("Parse many") {
    assertEquals(
      ProblemsParser(
        (NormalCase ++ Array("junk", "lines", "") ++ LinkInNewLine ++ Array(
          "another"
        ) ++ TitleInNewLine).iterator
      ),
      List(
        ProblemSummary("100. Short title", "Medium", "https://normallink.com"),
        ProblemSummary("200. Short title", "Medium", "https://longlink.com"),
        ProblemSummary(
          "300. Very long title",
          "Medium",
          "https://normallink.com"
        )
      )
    )
  }
}
