package hmics
import org.scalatest.FunSuite

import scala.io.Source

class ParseTest extends FunSuite {


  test("#1 - parse example"){
    val src = Source.fromString(
      "3 5 1 6\nTTTTT\nTMMMT\nTTTTT\n"
    )
    val expected =
      Input(3, 5, 1, 6,
        Vector(
          Vector(Tomato,Tomato,Tomato,Tomato,Tomato),
          Vector(Tomato,Mushroom,Mushroom,Mushroom,Tomato),
          Vector(Tomato,Tomato,Tomato,Tomato,Tomato)
        )
      )

    assert( Input.parse(src)  === expected)
  }

}
