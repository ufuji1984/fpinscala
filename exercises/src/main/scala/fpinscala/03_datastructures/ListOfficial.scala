object ListOfficial {

  // Ex. 3.24
  def hasSubsequence_mine[A](sup: List[A], sub: List[A]): Boolean = {

    def loop(supStartIdx: Int): Boolean =
      if (supStartIdx > sup.length-sub.length+1) false
      else search(supStartIdx, 0)

    def search(supStartIdx: Int, subIdx: Int): Boolean =
      if (subIdx >= sub.length) loop(supStartIdx+1)
      else compare(supStartIdx, subIdx, supStartIdx)

    def compare(supIdx: Int, subIdx: Int, supStartIdx: Int): Boolean =
      if (sup.apply(supIdx) != sub.apply(subIdx)) search(supStartIdx+1, 0)
      else {
        if (isLastToCompare(supIdx, supStartIdx)) true
        else compare(supIdx+1, subIdx+1, supStartIdx)
      }

    def isLastToCompare(supIdx: Int, supStartIdx: Int): Boolean =
      (supIdx - supStartIdx + 1) >= sub.length

    loop(0)
  }

}
