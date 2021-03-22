package u04lab.code

import u04lab.code.Lists._
import u04lab.code.Optionals._
import u04lab.code.Streams.Stream

import scala.util.Random

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  def reversed(): PowerIterator[A]
}

object PowerIterator {
  def apply[A](stream: Stream[A]): PowerIterator[A] = new PowerIteratorImpl(stream)

  private class PowerIteratorImpl[A](var stream: Stream[A]) extends PowerIterator[A] {

    private var _pastList: List[A] = List.Nil()

    override def next(): Option[A] = stream match {
      case Stream.Cons(h, t) => {
        _pastList = List.append(_pastList, List.Cons[A](h(), List.Nil()))
        stream = t()
        Option.Some(h())
      }
      case _ => Option.None()
    }

    override def allSoFar(): List[A] = _pastList
    override def reversed(): PowerIterator[A] = new PowerIteratorsFactoryImpl().fromList(List.reverse(_pastList))
  }
}

trait PowerIteratorsFactory {

  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: List[A]) : PowerIterator[A]
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] = PowerIterator[Int](Stream.iterate(start)(successive))
  override def fromList[A](list: List[A]): PowerIterator[A] = PowerIterator[A](List.toStream(list))
  override def randomBooleans(size: Int): PowerIterator[Boolean] = {
    val random:Random=new Random()
    PowerIterator[Boolean](Stream.take(Stream.generate(random.nextBoolean()))(size))
  }
}
