object MatchingBrackets {

  case class BracketsStack(brackets: List[Char], matchFailed: Boolean) {

    def increment(c: Char): BracketsStack = BracketsStack.increment(this, c)

    val isPaired: Boolean = brackets.isEmpty && !matchFailed
  }

  object EmptyBracketsStack$ extends BracketsStack(List.empty, false)

  object BracketsStack {

    def increment(bracketsStack: BracketsStack, char: Char): BracketsStack = {

      def matchBracket(Bracket: Char): BracketsStack = bracketsStack.brackets match {
        case Bracket :: tail => BracketsStack(tail, matchFailed = false)
        case _ => BracketsStack(bracketsStack.brackets, matchFailed = true)
      }

      char match {
        case c @ ('[' | '{' | '(') => BracketsStack(c :: bracketsStack.brackets, matchFailed = false)
        case ']'                   => matchBracket('[')
        case '}'                   => matchBracket('{')
        case ')'                   => matchBracket('(')
        case _                     => bracketsStack
      }
    }
  }

  def isPaired(input: String): Boolean = {

    @scala.annotation.tailrec
    def iter(in: List[Char], bracketsStack: BracketsStack): BracketsStack = in match {
      case _ if bracketsStack.matchFailed => bracketsStack
      case List()                            => bracketsStack
      case head :: tail                      => iter(tail, bracketsStack.increment(head))
    }

    val counts = iter(input.toList, EmptyBracketsStack$)
    counts.isPaired
  }
}