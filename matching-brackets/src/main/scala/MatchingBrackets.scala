object MatchingBrackets {

  case class Counts(nBracket: Int, nBrace: Int, nParen: Int) {

    def increment(c: Char): Counts = Counts.increment(this, c)

    val isPaired: Boolean = nBracket == 0 && nBrace == 0 && nParen == 0
    val cannotBePaired: Boolean = nBracket < 0 || nBrace < 0 || nParen < 0
  }

  object ZeroCounts extends Counts(0, 0, 0)

  object Counts {

    def increment(counts: Counts, c: Char): Counts = c match {
      case '[' => counts.copy(nBracket = counts.nBracket + 1)
      case ']' => counts.copy(nBracket = counts.nBracket - 1)
      case '{' => counts.copy(nBrace = counts.nBrace + 1)
      case '}' => counts.copy(nBrace = counts.nBrace - 1)
      case '(' => counts.copy(nParen = counts.nParen + 1)
      case ')' => counts.copy(nParen = counts.nParen - 1)
      case _   => counts
    }
  }

  def isPaired(input: String): Boolean = {

    @scala.annotation.tailrec
    def iter(in: List[Char], counts: Counts): Counts = in match {
      case _ if counts.cannotBePaired => counts
      case List()                     => counts
      case head :: tail               => iter(tail, counts.increment(head))
    }

    val counts = iter(input.toList, ZeroCounts)
    counts.isPaired
  }
}