object JavaCharacter {
  def letterGroups[A](p: A => Boolean, x: List[A]): List[(A, A)] =
    x match {
      case Nil => Nil
      case _ => {
        val (r, s) = x dropWhile (x => !p(x)) span p
        r match {
          case Nil => letterGroups(p, s)
          case h::t => (h, t.reverse.headOption getOrElse h) :: letterGroups(p, s)
        }
      }
    }

  def letterGroupsToHaskell[A](p: A => Boolean, x: List[A]): String =
    "[\n  " + (letterGroups(p, x) map {
      case (a, b) => "[" + (if(a == b) a else a + ".." + b) + "]"
    }).mkString("\n, ") + "\n]"
    
  def main(args: Array[String]) {   
    val l = (0 to 200000).toList
    val r = letterGroupsToHaskell[Int](Character.isLetter(_), l) 
    println(r)
  }
}

