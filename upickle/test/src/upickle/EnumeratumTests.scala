package upickle

import utest._

import scala.reflect.{ClassTag, classTag}

// https://github.com/lloydmeta/enumeratum stand-in
trait EnumEntry {

  def entryName: String
}

// https://github.com/lloydmeta/enumeratum stand-in
trait Enum[T <: EnumEntry] {

  def values: Seq[T]
}

/**
  * I'd like to make something like this.
  */
trait UPickleEnum[T <: EnumEntry] extends Enum[T] {

  implicit def rw[C <: T]: upickle.default.ReadWriter[C] = {
    upickle.default.readwriter[String]
      .bimap[C](
        _.entryName,
        str => values.find(_.entryName == str).get match {
          case child: C => child
          case _ => throw new Exception(s"wrong child")
        }
      )
  }
}

sealed abstract class Fruit(override val entryName: String) extends EnumEntry

object Fruit extends UPickleEnum[Fruit] {

  case object Peach extends Fruit("peach")

  case object Strawberry extends Fruit("strawberry")

  override val values = Seq(Peach, Strawberry)
}

object EnumeratumTests extends TestSuite {

  val tests = Tests {

    test("write") {
      test("Fruit") {
        // Passes, but requires explicit cast.
        upickle.default.write(Fruit.Peach: Fruit) ==> "\"peach\""
      }
      test("Peach") {
        // Can we remove the cast?
        // Fails:
        upickle.default.write(Fruit.Peach) ==> "\"peach\"" // {"$type":"upickle.Fruit.Peach"}
      }
    }
  }
}
