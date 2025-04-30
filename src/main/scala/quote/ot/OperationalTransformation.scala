package quote.ot

object OperationalTransformation:
  def transform(op1: Operation, op2: Operation): (Operation, Operation) = (op1, op2) match
    case (Insert(i1, s1), Insert(i2, s2)) =>
      if (i1 < i2)
        (Insert(i1, s1), Insert(i2 + s1.length, s2))  // a'=a, b' shifted right
      else if (i1 > i2)
        (Insert(i1 + s2.length, s1), Insert(i2, s2))  // a' shifted right, b'=b
      else
        // Tie-break (e.g., by client ID in a real system)
        (Insert(i1, s1), Insert(i2 + s1.length, s2))  // a wins, b' shifted

    case (Delete(i1, s1), Delete(i2, s2)) =>
      val end1 = i1 + s1.length
      val end2 = i2 + s2.length

      if (end1 <= i2)
        // a before b: no change
        (Delete(i1, s1), Delete(i2 - s1.length, s2))  // b' adjusted left
      else if (i1 >= end2)
        // a after b: a adjusted left
        (Delete(i1 - s2.length, s1), Delete(i2, s2))
      else
        // Overlapping deletions: subtract intersection
        val overlapStart = math.max(i1, i2)
        val overlapEnd = math.min(end1, end2)
        val overlapLen = overlapEnd - overlapStart

        val newS1 = if   i1 < i2   then s1.substring(0, i2 - i1) else ""
        val newS2 = if end2 > end1 then s2.substring(overlapEnd - i2) else ""

        (Delete(i1, newS1), Delete(i2, newS2))

    // Case 3: Insert vs Delete
    case (Insert(i1, s1), Delete(i2, s2)) =>
      if (i1 <= i2)
        (Insert(i1, s1), Delete(i2 + s1.length, s2))  // b' shifted right
      else if (i1 < i2 + s2.length)
        (Insert(i2, ""), Delete(i2, s2.take(i1 - i2) ++ s1 ++ s2.drop(i1 - i2)))
      else
        (Insert(i1 - s2.length, s1), Delete(i2, s2))  // a' shifted left

    // Case 4: Delete vs Insert
    case (Delete(i1, s1), Insert(i2, s2)) =>
      if (i1 >= i2)
        (Delete(i1 + s2.length, s1), Insert(i2, s2))  // a' shifted right
      else if (i2 >= i1 + s1.length)
        (Delete(i1, s1.take(i2 - i1) ++ s2 ++ s1.drop(i2 - i1)), Insert(i1, ""))
      else
        (Delete(i1, s1), Insert(i2 - s1.length, s2))  // b' shifted left

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
