package u04lab.code

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u04lab.code.Lists._
import u04lab.code.Optionals._

class PowerIteratorsTest {

  val factory = new PowerIteratorsFactoryImpl()

  @Test
  def testIncremental() {
    val pi = factory.incremental(5,_+2); // pi produce 5,7,9,11,13,...

    assertEquals(Option.of(5), pi.next());
    assertEquals(Option.of(7), pi.next());
    assertEquals(Option.of(9), pi.next());
    assertEquals(Option.of(11), pi.next());
    assertEquals(List.Cons(5, List.Cons(7, List.Cons(9, List.Cons(11,List.Nil())))), pi.allSoFar()); // elementi già prodotti
    for (i <- 0 until 10) {
      pi.next(); // procedo in avanti per un po'..
    }
    assertEquals(Option.of(33), pi.next()); // sono arrivato a 33
  }

  @Test
  def testRandom() {
    val pi = factory.randomBooleans(4);

    val b1:Boolean = Option.getOrElse(pi.next(),false)
    val b2:Boolean = Option.getOrElse(pi.next(),false)
    val b3:Boolean = Option.getOrElse(pi.next(),false)
    val b4:Boolean = Option.getOrElse(pi.next(),false)

    System.out.println(b1 + " " + b2 + " " + b3 + " " + b4)

    assertTrue(Option.isEmpty(pi.next()))
    assertEquals(List.Cons(b1, List.Cons(b2, List.Cons(b3, List.Cons(b4,List.Nil())))), pi.allSoFar());
  }

  @Test
  def testFromList() {
    val pi = factory.fromList(List.Cons("a", List.Cons("b", List.Cons("c",List.Nil()))));

    assertEquals(pi.next, Option.of("a"))
    assertEquals(pi.next, Option.of("b"))
    assertEquals(pi.allSoFar, List.Cons("a", List.Cons("b",List.Nil()))) // fin qui a,b

    assertEquals(pi.next, Option.of("c"))
    assertEquals(pi.allSoFar, List.Cons("a", List.Cons("b", List.Cons("c",List.Nil())))) // fin qui a,b,c

    assertTrue(Option.isEmpty(pi.next)) // non c'è più niente da produrre
  }

  @Test
  def testReversedFromList() {
    val pi = factory.fromList(List.Cons("a", List.Cons("b", List.Cons("c",List.Nil()))));

    assertEquals(pi.next, Option.of("a"))
    assertEquals(pi.next, Option.of("b"))

    val pi2 = pi.reversed()

    assertEquals(pi.next, Option.of("c"))
    assertTrue(Option.isEmpty(pi.next))

    assertEquals(pi2.next, Option.of("b"))
    assertEquals(pi2.next, Option.of("a"))
    assertEquals(pi2.allSoFar, List.Cons("b", List.Cons("a",List.Nil())))
    assertTrue(Option.isEmpty(pi2.next))
  }

  @Test
  def optionalTestReversedOnIncremental() {
    val pi = factory.incremental(0,_+1);

    assertEquals(Option.of(0), pi.next());
    assertEquals(Option.of(1), pi.next());
    assertEquals(Option.of(2), pi.next());
    assertEquals(Option.of(3), pi.next());

    val pi2 = pi.reversed()

    assertEquals(Option.of(3), pi2.next());
    assertEquals(Option.of(2), pi2.next());

    val pi3 = pi2.reversed()
    assertEquals(Option.of(2), pi3.next());
    assertEquals(Option.of(3), pi3.next());
    assertTrue(Option.isEmpty(pi3.next))
  }

}