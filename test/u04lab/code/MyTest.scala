package u04lab.code

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

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
}