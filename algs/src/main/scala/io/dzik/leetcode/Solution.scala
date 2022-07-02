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

  /** Title: 20. Valid Parentheses
    *
    * Link: https://leetcode.com/problems/valid-parentheses/
    *
    * Difficulty: Easy
    */
  def isValid(s: String): Boolean = {
    val q = mutable.ArrayDeque[Char]()
    val oToC = Map(
      '(' -> ')',
      '[' -> ']',
      '{' -> '}'
    )

    val isOpen: Char => Boolean = oToC.contains

    for (c <- s)
      if (isOpen(c)) {
        q.prepend(c)
      } else {
        if (q.isEmpty) return false
        val lastOpen = q.removeHead()
        if (oToC(lastOpen) != c) return false
      }

    q.isEmpty
  }

  /** Title: 21. Merge Two Sorted Lists
    *
    * Link: https://leetcode.com/problems/merge-two-sorted-lists/
    *
    * Difficulty: Easy
    */
  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
    var (x, y) = (l1, l2)
    val dummy = new ListNode()
    var node = dummy

    while (Option(x).isDefined && Option(y).isDefined) {
      if (x.x < y.x) {
        node.next = x
        x = x.next
      } else {
        node.next = y
        y = y.next
      }
      node = node.next
    }

    node.next = if (Option(x).isDefined) x else y

    dummy.next
  }

  /** Title: 22. Generate Parentheses
    *
    * Link: https://leetcode.com/problems/generate-parentheses/
    *
    * Difficulty: Medium
    */
  def generateParenthesis(n: Int): List[String] = {
    val res = new mutable.ArrayBuffer[String]()
    val sb = new StringBuilder()

    def inner(o: Int, c: Int): Unit =
      if (o == n && o == c) res += sb.toString()
      else {
        if (o < n && o >= c) {
          sb.append('(')
          inner(o + 1, c)
          sb.deleteCharAt(sb.length() - 1)
        }
        if (c < n && o > c) {
          sb.append(')')
          inner(o, c + 1)
          sb.deleteCharAt(sb.length() - 1)
        }
      }

    inner(0, 0)
    res.toList
  }

  /** Title: 26. Remove Duplicates from Sorted Array
    *
    * Link: https://leetcode.com/problems/remove-duplicates-from-sorted-array/
    *
    * Difficulty: Easy
    */
  def removeDuplicates(xs: Array[Int]): Int = {
    if (xs.isEmpty) return 0

    @tailrec
    def inner(l: Int, r: Int): Int =
      if (r == xs.length) l
      else {
        if (xs(r) != xs(r - 1)) {
          xs(l) = xs(r)
          inner(l + 1, r + 1)
        } else {
          inner(l, r + 1)
        }
      }

    inner(1, 1)
  }

  def removeDuplicatesIter(xs: Array[Int]): Int = {
    if (xs.isEmpty) return 0
    var l = 1
    var r = 1

    while (r < xs.length) {
      if (xs(r) != xs(r - 1)) {
        xs(l) = xs(r)
        l += 1
      }

      r += 1
    }

    l
  }

  /** Title: 27. Remove Element
    *
    * Link: https://leetcode.com/problems/remove-element/
    *
    * Difficulty: Easy
    */
  def removeElement(xs: Array[Int], toRemove: Int): Int = {
    @tailrec
    def inner(r: Int, w: Int): Int =
      if (r == xs.length) w
      else if (xs(r) != toRemove) {
        xs(w) = xs(r)
        inner(r + 1, w + 1)
      } else {
        inner(r + 1, w)
      }

    inner(0, 0)
  }

  def removeElementIter(xs: Array[Int], toRemove: Int): Int = {
    var w = 0

    for (r <- xs) {
      if (r != toRemove) {
        xs(w) = r
        w += 1
      }
    }

    w
  }

  /** Title: 28. Implement strStr()
    *
    * Link: https://leetcode.com/problems/implement-strstr/
    *
    * Difficulty: Easy
    */
  def strStr(a: String, b: String): Int = {
    if (b.isEmpty) return 0
    if (b.length > a.length) return -1

    for (i <- a.indices)
      if (a(i) == b(0)) {
        if (i + b.length <= a.length && a.substring(i, i + b.length) == b)
          return i
      }
    -1
  }

  /** Title: 31. Next Permutation
    *
    * Link: https://leetcode.com/problems/next-permutation/
    *
    * Difficulty: Medium
    */
  def nextPermutation(xs: Array[Int]): Unit = {
    def swap(arr: Array[Int], i: Int, j: Int) = {
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }

    var k = -1

    for (i <- 0 until xs.length - 1)
      if (xs(i) < xs(i + 1)) k = i
    var l = -1

    if (k != -1) {
      for (i <- k + 1 until xs.length)
        if (xs(k) < xs(i)) l = i

      swap(xs, k, l)
    }

    l = k + 1
    var r = xs.length - 1
    while (l < r) {
      swap(xs, l, r)
      l += 1
      r -= 1
    }
  }

  /** Title: 33. Search in Rotated Sorted Array
    *
    * Link: https://leetcode.com/problems/search-in-rotated-sorted-array/
    *
    * Difficulty: Medium
    */
  val NotFound = -1

  def search(xs: Array[Int], target: Int): Int = {
    var lo = 0
    var hi = xs.length - 1

    while (lo <= hi) {
      val mid = (hi + lo) / 2
      val x = xs(mid)

      if (x == target) return mid

      if (xs(lo) <= x) { // important <=
        if (xs(lo) <= target && target < x) hi = mid - 1
        else lo = mid + 1
      } else {
        if (xs(hi) >= target && target > x) lo = mid + 1
        else hi = mid - 1
      }
    }

    NotFound
  }

  /** Title: 34. Find First and Last Position of Element in Sorted Array
    *
    * Link:
    * https://leetcode.com/problems/find-first-and-last-position-of-element-in-sorted-array/
    *
    * Difficulty: Medium
    */
  def searchRange(xs: Array[Int], t: Int): Array[Int] = {
    if (xs.isEmpty) return Array(-1, -1)
    var lo = 0
    var hi = xs.length - 1
    var loRes = -1
    while (lo <= hi) {
      val mid = (lo + hi) / 2

      if (xs(mid) == t) loRes = mid

      if (xs(mid) < t) lo = mid + 1
      else hi = mid - 1
    }

    if (loRes == -1) return Array(-1, -1)

    lo = 0
    hi = xs.length - 1
    var hiRes = -1
    while (lo <= hi) {
      val mid = (lo + hi) / 2

      if (xs(mid) == t) hiRes = mid

      if (xs(mid) > t) hi = mid - 1
      else lo = mid + 1
    }

    Array(loRes, hiRes)
  }

  /** Title: 35. Search Insert Position
    *
    * Link: https://leetcode.com/problems/search-insert-position/
    *
    * Difficulty: Easy
    */
  def searchInsert(xs: Array[Int], x: Int): Int = {
    var lo = 0
    var hi = xs.length - 1

    while (lo <= hi) {
      val mid = (lo + hi) / 2

      if (xs(mid) == x) return mid
      else if (xs(mid) > x) hi = mid - 1
      else lo = mid + 1
    }

    lo
  }

  /** Title: 36. Valid Sudoku
    *
    * Link: https://leetcode.com/problems/valid-sudoku/
    *
    * Difficulty: Medium
    */
  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
    def hasUniqueDigits(xs: Seq[Char]): Boolean = {
      val digits = xs.filter(_.isDigit)

      digits.toSet.size == digits.length
    }

    def isValidRow(r: Int): Boolean = hasUniqueDigits(board(r))

    def isValidColumn(c: Int) = hasUniqueDigits(board.map(_(c)))

    def isValidSquare(ulr: Int, ulc: Int): Boolean = {
      val square = for {
        row <- ulr until ulr + 3
        col <- ulc until ulc + 3
      } yield board(row)(col)

      hasUniqueDigits(square)
    }

    board.indices.forall(isValidRow) &&
    board.indices.forall(isValidColumn) &&
    (for {
      r <- Seq(0, 3, 6)
      c <- Seq(0, 3, 6)
    } yield (r, c)).forall(xy => isValidSquare(xy._1, xy._2))
  }

  /** Title: 38. Count and Say
    *
    * Link: https://leetcode.com/problems/count-and-say/
    *
    * Difficulty: Medium
    */
  def countAndSay(n: Int): String = {
    def inner(prev: StringBuilder, j: Int): String = {
      if (j == n) {
        prev.toString()
      } else {
        var i = 0
        val cur = new mutable.StringBuilder()

        while (i < prev.length()) {
          val what = prev(i)
          var cnt = 0

          while (cnt == 0 || i < prev.length() && prev(i) == prev(i - 1)) {
            cnt += 1
            i += 1
          }

          cur ++= cnt.toString
          cur += what
        }

        inner(cur, j + 1)
      }
    }

    inner(new mutable.StringBuilder("1"), 1)
  }

  /** Title: 39. Combination Sum
    *
    * Link: https://leetcode.com/problems/combination-sum/
    *
    * Difficulty: Medium
    */
  def combinationSum(xs2: Array[Int], target: Int): List[List[Int]] = {
    val res = mutable.Set.empty[List[Int]]

    val xs = xs2.sorted

    def inner(start: Int, left: Int, cur: mutable.ArrayDeque[Int]): Unit =
      if (left < 0) ()
      else if (left == 0) res += cur.toList
      else {
        (start until xs.length).foreach { i =>
          cur += xs(i)
          inner(i, left - xs(i), cur)
          cur.removeLast()
        }
      }

    inner(0, target, mutable.ArrayDeque.empty[Int])
    res.toList
  }

  /** Title: 40. Combination Sum II
    *
    * Link: https://leetcode.com/problems/combination-sum-ii/
    *
    * Difficulty: Medium
    */
  def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
    val xs = candidates.filter(_ <= target).sorted
    val res = mutable.Set.empty[List[Int]]
    val acc = mutable.ArrayDeque.empty[Int]

    def inner(i: Int, sum: Int): Unit =
      if (sum == target) res += acc.toList
      else {
        var j = i
        while (j < xs.length && xs(j) + sum <= target)
          if (j > i && xs(j) == xs(j - 1)) j += 1
          else {
            acc += xs(j)
            inner(j + 1, sum + xs(j))
            acc.removeLast()

            j += 1
          }
      }

    inner(0, 0)

    res.toList
  }

  /** Title: 42. Trapping Rain Water
    *
    * Link: https://leetcode.com/problems/trapping-rain-water/
    *
    * Difficulty: Hard
    */
  def trap(hs: Array[Int]): Int = {
    val largerToTheLeft = hs
      .foldLeft((List.empty[Int], 0)) { case ((acc, m), h) =>
        val maxTillNow = m max h
        (maxTillNow :: acc, maxTillNow)
      }
      ._1
      .reverse

    val largerToTheRight = hs
      .foldRight((List.empty[Int], 0)) { case (h, (acc, m)) =>
        val maxTillNow = m max h
        (maxTillNow :: acc, maxTillNow)
      }
      ._1

    ((largerToTheLeft zip largerToTheRight) zip hs).foldLeft(0) {
      case (s, ((l, r), h)) => s + ((l min r) - h)
    }
  }

  /** Title: 43. Multiply Strings
    *
    * Link: https://leetcode.com/problems/multiply-strings/
    *
    * Difficulty: Medium
    */
  def multiply(a: String, b: String): String = {
    if (a == "0" || b == "0") return "0"
    val temp = Array.ofDim[Int](b.length + a.length)

    for {
      i <- a.indices
      j <- b.indices
    } {
      val x =
        (a(a.length - 1 - i) - '0') * (b(b.length - 1 - j) - '0') + temp(i + j)

      temp(j + i) = x % 10
      temp(j + i + 1) += (x / 10)
    }

    temp.reverse.dropWhile(_ == 0).mkString
  }

  /** Title: 45. Jump Game II
    *
    * Link: https://leetcode.com/problems/jump-game-ii/
    *
    * Difficulty: Medium
    */
  def jump(xs: Array[Int]): Int = {
    val N = xs.length
    val dp = Array.fill(N)(Int.MaxValue)

    dp(0) = 0

    for (i <- 0 until N - 1) {
      val M = xs(i) + i
      for (j <- i + 1 to (M min N - 1)) {
        dp(j) = (dp(i) + 1) min dp(j)
      }
    }

    dp.last
  }

  /** Title: 46. Permutations
    *
    * Link: https://leetcode.com/problems/permutations/
    *
    * Difficulty: Medium
    */
  def permute(xs: Array[Int]): List[List[Int]] = {
    val res = mutable.ListBuffer.empty[List[Int]]

    def inner(usedIndices: Set[Int], acc: List[Int]): Unit =
      if (usedIndices.isEmpty) res += acc
      else {
        for (i <- usedIndices)
          inner(usedIndices - i, xs(i) :: acc)
      }

    inner(xs.indices.toSet, Nil)

    res.toList
  }

  /** Title: 47. Permutations II
    *
    * Link: https://leetcode.com/problems/permutations-ii/
    *
    * Difficulty: Medium
    */
  def permuteUnique(xs: Array[Int]): List[List[Int]] = {
    val res = mutable.Set.empty[List[Int]]

    def inner(indices: Set[Int], current: List[Int]): Unit =
      if (indices.isEmpty) res += current
      else {
        for (i <- indices)
          inner(indices - i, xs(i) :: current)
      }

    inner(xs.indices.toSet, Nil)

    res.toList
  }

  /** Title: 48. Rotate Image
    *
    * Link: https://leetcode.com/problems/rotate-image/
    *
    * Difficulty: Medium
    */
  def rotate(m: Array[Array[Int]]): Unit = {
    val n = m.length

    @tailrec
    def inner(level: Int): Unit =
      if (level == n / 2) ()
      else {
        for (k <- 0 to n - (2 * (level + 1))) {
          val l = level
          val r = n - 1 - level

          val (ul, ur, lr, ll) = (
            m(l)(l + k),
            m(l + k)(r),
            m(r)(r - k),
            m(r - k)(l)
          )

          m(l)(l + k) = ll
          m(l + k)(r) = ul
          m(r)(r - k) = ur
          m(r - k)(l) = lr
        }

        inner(level + 1)
      }

    inner(0)
  }

  /** Title: 49. Group Anagrams
    *
    * Link: https://leetcode.com/problems/group-anagrams/
    *
    * Difficulty: Medium
    */
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    strs
      .groupBy(_.sorted)
      .view
      .mapValues(_.toList)
      .values
      .toList
  }

  /** Title: 50. Pow(x, n)
    *
    * Link: https://leetcode.com/problems/powx-n/
    *
    * Difficulty: Medium
    */
  def myPow(x: Double, n: Int): Double = {
    def inner(x: Double, n: Long): Double =
      if (n == 0) 1
      else if (n == 1) x
      else if (n == 2) x * x
      else if (n % 2 == 0) {
        val half = inner(x, n / 2)
        half * half
      } else {
        val half = inner(x, (n - 1) / 2)
        x * half * half
      }

    if (n < 0) 1 / inner(x, -(n.toLong))
    else inner(x, n)
  }
}
