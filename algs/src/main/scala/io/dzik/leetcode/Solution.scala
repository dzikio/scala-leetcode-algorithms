package io.dzik.leetcode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

import io.dzik.leetcode.LeetCodeApi.ListNode
import io.dzik.utils.Exceptions

object Solution extends App {

  /** Title: 1. Two Sum
    *
    * Difficulty: Easy
    *
    * Link: https://leetcode.com/problems/two-sum/
    */
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val xToI = nums.zipWithIndex.toMap

    nums.zipWithIndex
      .find { case (x, i) => xToI.get(target - x).exists(_ > i) }
      .map { case (x, i) => Array(i, xToI(target - x)) }
      .getOrElse(throw Exceptions.BadConditions)
  }

  def twoSumImperative(nums: Array[Int], target: Int): Array[Int] = {
    val xToI = nums.zipWithIndex.toMap

    for { i <- nums.indices } {
      val x = nums(i)
      val missing = target - x

      if (xToI.get(missing).exists(_ > i)) return Array(i, xToI(missing))
    }

    throw Exceptions.BadConditions
  }

  /** Title: 2. Add Two Numbers
    *
    * Difficulty: Medium
    *
    * Link: https://leetcode.com/problems/add-two-numbers/
    */
  def addTwoNumbers(w: ListNode, v: ListNode): ListNode = {
    @tailrec
    def inner(_l1: ListNode, _l2: ListNode, carry: Int, acc: ListNode): Unit =
      if (Option(_l1).isEmpty && Option(_l2).isEmpty) {
        if (carry != 0) acc.next = new ListNode(1)
      } else {
        val (x, l1) = getAndAdvance(_l1)
        val (y, l2) = getAndAdvance(_l2)

        val sum = x + y + carry
        val node = new ListNode(sum % 10)
        acc.next = node

        inner(l1, l2, sum / 10, acc.next)
      }

    val dummy = new ListNode()
    inner(w, v, 0, dummy)
    dummy.next
  }

  private def next(l: ListNode): ListNode = Option(l).map(_.next).orNull

  private def getAndAdvance(l: ListNode): (Int, ListNode) = {
    val nextNode = next(l)

    (Option(l).map(_.x).getOrElse(0), nextNode)
  }

  /** Title: 3. Longest Substring Without Repeating Characters
    *
    * Difficulty: Medium
    *
    * Link:
    * https://leetcode.com/problems/longest-substring-without-repeating-characters/
    */
  def lengthOfLongestSubstring(s: String): Int = {
    var res = 0
    var l = 0

    val lastOccurrence = mutable.Map.empty[Char, Int]

    s.zipWithIndex.foreach { case (c, i) =>
      if (lastOccurrence.contains(c) && lastOccurrence(c) >= l) {
        l = lastOccurrence(c) + 1 // key insight
      }

      lastOccurrence += (c -> i)
      res = res max (i - l + 1)
    }

    res
  }

  /** Title: 5. Longest Palindromic Substring
    *
    * Difficulty: Medium
    *
    * Link: https://leetcode.com/problems/longest-palindromic-substring/
    */
  def longestPalindrome(s: String): String = {
    val N = s.length

    @tailrec
    def expand(l: Int, r: Int): (Int, Int) =
      if (l >= 0 && r < N && s(l) == s(r)) expand(l - 1, r + 1)
      else {
        val prevL = l + 1
        val len = r - prevL

        (prevL, len)
      }

    val (start, len) = (for (i <- s.indices)
      yield Seq(expand(i, i), expand(i, i + 1))
        .maxBy({ case (_, l) => l }))
      .maxBy({ case (_, l) => l })

    s.substring(start, start + len)
  }

  /** Title: 6. ZigZag Conversion
    *
    * Link: https://leetcode.com/problems/zigzag-conversion/
    *
    * Difficulty: Medium
    */
  def convert(s: String, rows: Int): String = {
    if (rows == 1) return s

    var down = true
    val bufs = List.fill(rows)(new StringBuilder())
    var r = 0
    for (i <- s.indices) {
      bufs(r).append(s(i))

      if (down && r == rows - 1) {
        down = false
      } else if (!down && r == 0) {
        down = true
      }

      if (down) r += 1
      else r -= 1
    }

    bufs.mkString
  }

  /** Title: 7. Reverse Integer
    *
    * Difficulty: Easy
    *
    * Link: https://leetcode.com/problems/reverse-integer/
    */
  def reverse(x: Int): Int = {
    val nonNegative = x >= 0
    val parsingFailed = 0
    val candidate = x.abs.toString.reverse

    Try {
      candidate.toInt
    } match {
      case Failure(_) => parsingFailed
      case Success(v) => if (nonNegative) v else -v
    }
  }

  /** Title: 9. Palindrome Number
    *
    * Link: https://leetcode.com/problems/palindrome-number/
    *
    * Difficulty: Easy
    */
  def isPalindrome(x: Int): Boolean = x.toString == x.toString.reverse

  /** Title: 11. Container With Most Water
    *
    * Link: https://leetcode.com/problems/container-with-most-water/
    *
    * Difficulty: Medium
    */
  def maxArea(heights: Array[Int]): Int = {
    @tailrec
    def inner(l: Int, r: Int, maxSoFar: Int): Int =
      if (l >= r) maxSoFar
      else {
        val L = heights(l)
        val R = heights(r)
        val s = (L min R) * (r - l)

        if (L < R) inner(l + 1, r, maxSoFar max s)
        else inner(l, r - 1, maxSoFar max s)
      }

    inner(0, heights.length - 1, 0)
  }

  /** Title: 12. Integer to Roman
    *
    * Link: https://leetcode.com/problems/integer-to-roman/
    *
    * Difficulty: Medium
    */
  def intToRoman(num: Int): String = {
    val rToI = Map(
      "I" -> 1,
      "IV" -> 4,
      "V" -> 5,
      "IX" -> 9,
      "X" -> 10,
      "XL" -> 40,
      "L" -> 50,
      "XC" -> 90,
      "C" -> 100,
      "CD" -> 400,
      "D" -> 500,
      "CM" -> 900,
      "M" -> 1000
    )
    val roman = Array(
      "M",
      "CM",
      "D",
      "CD",
      "C",
      "XC",
      "L",
      "XL",
      "X",
      "IX",
      "V",
      "IV",
      "I"
    )

    var rest = num
    var i = 0
    val sb = new StringBuilder()

    while (rest != 0) {
      val r = roman(i)
      if (rest >= rToI(r)) {
        sb.append(r)
        rest -= rToI(r)
      } else {
        i += 1
      }
    }

    sb.toString
  }

  /** Title: 13. Roman to Integer
    *
    * Link: https://leetcode.com/problems/roman-to-integer/
    *
    * Difficulty: Easy
    */
  def romanToInt(s: String): Int = {
    val rToI = Map(
      "I" -> 1,
      "IV" -> 4,
      "V" -> 5,
      "IX" -> 9,
      "X" -> 10,
      "XL" -> 40,
      "L" -> 50,
      "XC" -> 90,
      "C" -> 100,
      "CD" -> 400,
      "D" -> 500,
      "CM" -> 900,
      "M" -> 1000
    )
    val roman = Array(
      "M",
      "CM",
      "D",
      "CD",
      "C",
      "XC",
      "L",
      "XL",
      "X",
      "IX",
      "V",
      "IV",
      "I"
    )

    @tailrec
    def inner(s: String, i: Int, acc: Int): Int =
      if (s == "") acc
      else if (s.startsWith(roman(i))) {
        val r = roman(i)
        inner(s.drop(r.length), i, acc + rToI(r))
      } else {
        inner(s, i + 1, acc)
      }

    inner(s, 0, 0)
  }

  /** Title: 14. Longest Common Prefix
    *
    * Link: https://leetcode.com/problems/longest-common-prefix/
    *
    * Difficulty: Easy
    */
  def longestCommonPrefix(strs: Array[String]): String = {
    val sorted = strs.sorted
    val (h, l) = (sorted.head, sorted.last)

    val L = (h.length min l.length)
    for (i <- 0 until L)
      if (h(i) != l(i)) return h.substring(0, i)

    h.substring(0, L)
  }

  /** Title: 15. 3Sum
    *
    * Link: https://leetcode.com/problems/3sum/
    *
    * Difficulty: Medium
    */
  def threeSum(xs: Array[Int]): List[List[Int]] = {
    val xss = xs.sorted
    val res = scala.collection.mutable.Set[List[Int]]()
    val N = xss.length

    for (i <- xss.indices)
      if (i == 0 || xss(i) != xss(i - 1)) {
        var l = i + 1
        var r = N - 1
        while (l < N && l < r) {
          val sum = xss(i) + xss(l) + xss(r)

          if (sum > 0) r -= 1
          else if (sum < 0) l += 1
          else {
            res += List(xss(i), xss(l), xss(r))
            l += 1
            r -= 1
          }

          while (sum < 0 && l < N && l < r && xss(l) == xss(l - 1)) l += 1
          while (sum > 0 && r > i && l < r && xss(r) == xss(r + 1)) r -= 1
        }
      }

    res.toList
  }

  /** Title: 16. 3Sum Closest
    *
    * Link: https://leetcode.com/problems/3sum-closest/
    *
    * Difficulty: Medium
    */
  def threeSumClosest(xs: Array[Int], x: Int): Int = {
    val xss = xs.sorted
    val N = xss.length
    var closest = xss(0) + xss(1) + xss(2)

    for (i <- xss.indices)
      if (i == 0 || xss(i) != xss(i - 1)) {
        var l = i + 1
        var r = N - 1
        while (l < N && l < r) {
          val sum = xss(i) + xss(l) + xss(r)

          if ((sum - x).abs < (closest - x).abs) closest = sum
          if (sum > x) r -= 1
          else if (sum < x) l += 1
          else return x

          while (sum < x && l < N && l < r && xss(l) == xss(l - 1)) l += 1
          while (sum > x && r > i && l < r && xss(r) == xss(r + 1)) r -= 1
        }
      }

    closest
  }

  /** Title: 17. Letter Combinations of a Phone Number
    *
    * Link: https://leetcode.com/problems/letter-combinations-of-a-phone-number/
    *
    * Difficulty: Medium
    */
  def letterCombinations(digits: String): List[String] = {
    if (digits.isEmpty) return Nil

    val dToLetters = Map(
      '2' -> "abc",
      '3' -> "def",
      '4' -> "ghi",
      '5' -> "jkl",
      '6' -> "mno",
      '7' -> "pqrs",
      '8' -> "tuv",
      '9' -> "wxyz"
    )

    @tailrec
    def inner(s: String, acc: List[String]): List[String] =
      if (s.isEmpty) acc
      else {
        val next = dToLetters(s.head)
          .flatMap(c => acc.map(_ + c))
          .toList

        inner(s.tail, next)
      }

    inner(digits, List(""))
  }
}
