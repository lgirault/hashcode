package hmics

import java.net.URI

import scala.io.Source
import Input._

sealed trait Ingredient
case object Tomato extends Ingredient
case object Mushroom extends Ingredient


case class Input(numRows:Int,
                 numCols:Int,
                 minIngredientPerSlice: Int,
                 sliceMaxCells: Int,
                 pizza: Pizza)
object Input {
  type Pizza = Vector[Vector[Ingredient]]

  def read(uri: URI) : Input = parse(Source.fromFile(uri, "ASCII"))

  def parse(source: Source) = {
    val bufIterator = source.getLines()

    val header = bufIterator.next().split(" ")

    val Array(numRows, numCols, minIngredientPerSlice, sliceMaxCells) = header

    val pizz : Pizza = bufIterator.foldLeft(Vector.empty[Vector[Ingredient]]){
      case (p, line) =>
        p :+ line.foldLeft(Vector.empty[Ingredient]){
          case (l, 'T') => l :+ Tomato
          case (l, 'M') => l :+ Mushroom
          case (_, c) => throw new Exception(s"unexpected ingredient ${c}")
        }
    }
    Input(
      numRows.toInt,
      numCols.toInt,
      minIngredientPerSlice.toInt,
      sliceMaxCells.toInt,
      pizz)

  }

}


case class Slice(p1: (Int,Int), p2: (Int, Int))

object Main {

  def count(r: Int, c: Int, r1: Int, c1: Int)
           (implicit input: Input): (Int, Int) = {

    var r0 = r
    var c0 = c
    var t = 0
    var m = 0

    while(r0 <= r1){
      while(c0 <= c1){
        if(input.pizza(r0)(c0) == Tomato)
          t += 1
        else m += 1

        c0 += 1
      }
      r0 += 1
    }

    (t, m)
  }


  def area(r: Int, c: Int, r1: Int, c1: Int): Int =
    (r1 - r) * (c1 - c)

  def cutSquareSlice(startingPoint: (Int, Int))
                    (implicit input: Input) :Option[Slice] = {
    import input._
     val (r, c) = startingPoint

     val (t0, m0) =
       if(input.pizza(r)(c) == Tomato) (1, 0)
       else (0, 1)

    def aux(r1: Int = r, c1: Int = c,
            tomato: Int = t0, mushroom: Int = m0,
            numCells: Int = 1,
            step: Int = 0) : Option[Slice] =
      if(numCells > sliceMaxCells || r1 > numRows || c1 > numCols) None
      else if(tomato >= minIngredientPerSlice && mushroom >= minIngredientPerSlice)
        Some( Slice(startingPoint, (r1, c1)) )
      else if(step % 2 == 0) {
        val c2 = c1 + 1

        val (ts, ms) = count(r1, c1, r1, c2)

        aux(r1, c2,
          tomato + ts,
          mushroom + ms,
          numCells + ts + ms,
          step + 1)
      }
      else {
        val r2 = r1 + 1

        val (ts, ms) = count(r1, c1, r2, c1)

        aux(r2, c1,
          tomato + ts,
          mushroom + ms,
          numCells + ts + ms,
          step + 1)
      }
    aux()
  }

  def cut(input: Input) : List[Slice] = {
    import input._
    def aux(r: Int, c: Int, r1: Int, c1: Int) = {
      if(area(r, c, r1, c1) > sliceMaxCells)
    }

  }

  def main(args: Array[String]) = {

    val exUrl = getClass.getClassLoader.getResource("example.in")

    println(Input.read(exUrl.toURI))

  }

}
