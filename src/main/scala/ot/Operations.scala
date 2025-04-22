package ot

import io.circe._
import io.circe.generic.semiauto._

sealed trait Operation derives Codec.AsObject
case class Insert(index: Int, str: String) extends Operation
case class Delete(index: Int, length: Int) extends Operation
