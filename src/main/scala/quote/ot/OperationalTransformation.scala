package quote.ot

object OperationalTransformation:
  def transform(a: Operation, b: Operation): (Operation, Operation) = (a, b) match
    case (Insert(aIndex, aStr), Insert(bIndex, bStr)) =>
      (Insert(aIndex + bStr.length, aStr), Insert(bIndex + aStr.length, bStr))
    case (Insert(aIndex, aStr), Delete(bIndex, bLen)) =>
      (Insert(aIndex - bLen, aStr), Delete(bIndex + aStr.length, bLen))
    case (Delete(aIndex, aLen), Insert(bIndex, bStr)) =>
      (Delete(aIndex + bStr.length, aLen), Insert(bIndex - aLen, bStr))
    case (Delete(aIndex, aLen), Delete(bIndex, bLen)) =>
      (Delete(aIndex - bLen, aLen), Delete(bIndex - aLen, bLen))

  def transform(a: List[Operation], b: List[Operation]): (List[Operation], List[Operation]) =
    def transformList(o: Operation, b: List[Operation]): (Operation, List[Operation]) =
      b match
        case Nil => (o, Nil)
        case p :: psTail =>
          val (o1, p1)  = transform(o, p)
          val (o2, ps1) = transformList(o1, psTail)
          (o2, p1 :: ps1)

    a match
      case Nil => (Nil, b)
      case o :: osTail =>
        val (o1, b1) = transformList(o, b)
        val (a1, b2) = transform(osTail, b1)
        (o1 :: a1, b2)

