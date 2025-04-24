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

