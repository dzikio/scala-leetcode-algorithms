package io.dzik.leetcode

object Utils {
  class ListNode(var x: Int = 0, var next: ListNode = null)

  val BadConditions = new IllegalArgumentException(
    "Does not satisfy conditions"
  )
}
