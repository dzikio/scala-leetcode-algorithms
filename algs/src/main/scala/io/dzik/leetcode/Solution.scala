package io.dzik.leetcode

import scala.annotation.tailrec
import scala.collection.mutable

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
}
