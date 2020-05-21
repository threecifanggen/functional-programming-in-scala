package listenhence

object ListEnhence {
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
        case (Nil, Nil) => true
        case (head::tl, heads::Nil) => (head == heads)
        case (head::tl, heads::tls) => if (head == heads) hasSubsequence(sup.tail, sub.tail) else (hasSubsequence(sup, sub.tail) || hasSubsequence(sup.tail, sub))
    }
}