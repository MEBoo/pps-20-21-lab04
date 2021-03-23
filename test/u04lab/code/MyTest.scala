package u04lab.code

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class MyTest {

  @Test
  def testComplex() {

    val a = Array(Complex(10,20), Complex(1,1), Complex(7,0))
    val c = a(0) + a(1) + a(2)
    assertEquals(18.0,c.re);
    assertEquals(21.0,c.im);

    // equals test
    assertEquals(Complex(18.0,21.0),c);
    assertTrue(Complex(18.0,21.0)==c);

    val c2 = a(0) * a(1)
    assertEquals(-10.0,c2.re);
    assertEquals(30.0,c2.im);

    // equals test
    assertEquals(Complex(-10.0,30.0),c2);

    // toString check
    assertTrue("ComplexImpl(-10.0,30.0)"==c2.toString());
  }

  @Test
  def testStudentAndCourses() {

    import u04lab.code.Lists.List._
    
    val cPPS = Course("PPS","Viroli")
    val cPCD = Course("PCD","Ricci")
    val cSDR = Course("SDR","D'Angelo")
    val s1 = Student("mario",2015)
    val s2 = Student("gino",2016)
    val s3 = Student("rino") //defaults to 2017

    s1.enrolling(cPPS)
    s1.enrolling(cPCD)

    s2.enrolling(cPPS)

    s3.enrolling(cPPS)
    s3.enrolling(cPCD)
    s3.enrolling(cSDR)

    assertEquals(Cons("PCD",Cons("PPS",Nil())),s1.courses)
    assertEquals(Cons("PPS",Nil()),s2.courses)
    assertEquals(Cons("SDR",Cons("PCD",Cons("PPS",Nil()))),s3.courses)

    assertTrue(s1.hasTeacher("Ricci"))
    assertFalse(s1.hasTeacher("D'Angelo"))

    val s4 = Student("gigio")
    s4.enrolling(cPPS,cPCD,cSDR)

    assertEquals(Cons("SDR",Cons("PCD",Cons("PPS",Nil()))),s4.courses)
  }

  @Test
  def testSameTeacher(): Unit = {
    val c1 = Course("c1","Viroli")
    val c2 = Course("c2","Viroli")
    val c3 = Course("c3","Viroli")

    import u04lab.code.Lists._
    val courses = List ( c1 , c2 , c3 )

    courses match {
      case SameTeacher( "Viroli" ) => assertTrue(true)    // così verifico anche che il teacher risultante dal match sia esattamente Viroli
      case _ => fail ()
    }
  }

  @Test
  def testNotSameTeacher(): Unit = {
    val c1 = Course("c1","Viroli")
    val c2 = Course("c2","Viroli")
    val c3 = Course("c3","Viroli")
    val c4 = Course("c4","Ciccio")

    import u04lab.code.Lists._
    val courses = List ( c1 , c2 , c4, c3 )

    courses match {
      case SameTeacher( _ ) => fail()
      case _ => assertTrue(true)
    }
  }

  @Test
  def testListFactory() {

    import u04lab.code.Lists.List._
    import u04lab.code.Lists._

    val list=List(1,2,3,4,5,10);

    assertEquals(Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(10,Nil())))))),list)

    assertTrue(List.contains(list)(10))
  }
}