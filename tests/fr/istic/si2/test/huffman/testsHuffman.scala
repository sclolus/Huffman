package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.testerApp._
import fr.istic.si2.huffman._
import fr.istic.si2.huffman.Utils._
import fr.istic.si2.huffman.ConstructionCode._

import Utils._

class TestsHuffman {

  val _ = new AppInit(HuffmanApp0) // Ne pas supprimer cette ligne.

  // TODO A vous de compléter les jeux de test !
  // Vous devrez tester soigneusement chacune de vos fonctions.

  /**
   * Test du decodage d'un caractère V0
   */
  @Test
  def testDecodeSymbolv0() {
    val h = Noeud(
      1.00,
      Feuille(0.5, 'c'),
      Noeud(
        0.5,
        Feuille(0.25, 'a'),
        Feuille(0.25, 'b')))

    assertEquals(None, decodeSymbolv0(h, Nil))
    assertEquals(None, decodeSymbolv0(h, Zero :: One :: Nil))
    assertEquals(None, decodeSymbolv0(h, One :: Nil))

    assertEquals(Some('c'), decodeSymbolv0(h, Zero :: Nil))
    assertEquals(Some('b'), decodeSymbolv0(h, One :: One :: Nil))
  }

  /**
   * Test de encodeSymbol()
   */
  @Test
  def testEncodeSymbol() {
    val h = Noeud(
      1.00,
      Feuille(0.5, 'c'),
      Noeud(
        0.5,
        Feuille(0.25, 'a'),
        Feuille(0.25, 'b')))

    assertEquals(Some(Zero :: Nil), encodeSymbol('c', h))
    assertEquals(Some(One :: Zero :: Nil), encodeSymbol('a', h))
    assertEquals(Some(Nil), encodeSymbol('a', Feuille(1.00, 'a')))

    assertEquals(None, encodeSymbol('d', h))
    assertEquals(None, encodeSymbol('d', Feuille(1.00, 'a')))

  }

  /**
   * Test de listBitToString()
   */
  @Test
  def testlistBitToString() {
    assertEquals("", listBitToString(Nil))
    assertEquals("0", listBitToString(Zero :: Nil))
    assertEquals("1", listBitToString(One :: Nil))
    assertEquals("010", listBitToString(Zero :: One :: Zero :: Nil))
  }

  /**
   * Test de encodeList()
   */
  @Test
  def testEncodeList() {
    val h = Noeud(
      1.00,
      Feuille(0.5, 'c'),
      Noeud(
        0.5,
        Feuille(0.25, 'a'),
        Feuille(0.25, 'b')))

    assertEquals(Zero :: Nil, encodeList('c' :: Nil, h))
    assertEquals(
      Zero :: One :: Zero :: One :: One :: Nil,
      encodeList('c' :: 'a' :: 'b' :: Nil, h))

    assertEquals(Nil, encodeList('d' :: 'f' :: 'e' :: Nil, h))
    assertEquals(Nil, encodeList('a' :: 'a' :: 'a' :: Nil, Feuille(1.00, 'a')))

  }

  /**
   * Test de decodeSymbol()
   */
  @Test
  def testDecodeSymbol() {
    val h: Huffman = Noeud(
      1.00,
      Noeud(
        0.57,
        Feuille(0.25, 'a'),
        Noeud(
          0.32,
          Feuille(0.18, 'c'),
          Feuille(0.14, 'd'))),
      Noeud(
        0.43,
        Feuille(0.21, 'b'),
        Noeud(
          0.22,
          Noeud(
            0.13,
            Feuille(0.07, 'f'),
            Feuille(0.06, 'g')),
          Feuille(0.09, 'e'))))

    assertEquals((Some('a'), Nil), decodeSymbol(h, Zero :: Zero :: Nil))
    assertEquals((Some('b'), One :: One :: Zero :: One :: Nil), decodeSymbol(h, One :: Zero :: One :: One :: Zero :: One :: Nil))
    assertEquals((None, Nil), decodeSymbol(h, Zero :: One :: Nil))
    // TODO : ascertain that the case (None, NonEmptyList) doesn't exist
  }

  /**
   * Test de decode()
   */
  @Test
  def testDecode() {
    val h: Huffman = Noeud(
      1.00,
      Noeud(
        0.57,
        Feuille(0.25, 'a'),
        Noeud(
          0.32,
          Feuille(0.18, 'c'),
          Feuille(0.14, 'd'))),
      Noeud(
        0.43,
        Feuille(0.21, 'b'),
        Noeud(
          0.22,
          Noeud(
            0.13,
            Feuille(0.07, 'f'),
            Feuille(0.06, 'g')),
          Feuille(0.09, 'e'))))

    assertEquals(None, decode(Nil, h)) // TODO : Is it Some("") Or Nil ?
    assertEquals(Some("bg"), decode(One :: Zero :: One :: One :: Zero :: One :: Nil, h))
    assertEquals(None, decode(One :: Zero :: One :: Nil, h))

    // TODO : ascertain that there is no cases of badly encoded caracteres in the middle of a string
    // That is, that the only case of failure of decoding a char is at the end of the list of bits
  }

  /**
   * Test de initHuffman()
   */
  @Test
  def testInitHuffman() {
    assertEquals(Nil, initHuffman(Nil))
    assertEquals(List(Feuille(0.5, 'a'), Feuille(0.5, 'b')), initHuffman(('a', 0.5) :: ('b', 0.5) :: Nil))
    assertEquals(List(Feuille(0.5, 'a')), initHuffman(('a', 0.5) :: Nil))
  }

  /**
   * Test de triSelonFreq()
   */
  @Test
  def testTriSelonFreq() {
    val l = Feuille(0.66, 'c') :: Feuille(0.33, 'b') :: Feuille(0.16, 'c') :: Nil
    val expected = Feuille(0.16, 'c') :: Feuille(0.33, 'b') :: Feuille(0.66, 'c') :: Nil

    assertEquals(Nil, triSelonFreq(Nil))
    assertEquals(Feuille(1.0, 'a') :: Nil, triSelonFreq(Feuille(1.0, 'a') :: Nil))
    assertEquals(expected, triSelonFreq(l))
    // TODO: Some more
  }

  /**
   * Test de uneFusion()
   */
  @Test
  def testUneFusion() {
  // TODO:
  }

  /**
   * Test de fusion()
   */
  @Test
  def testFusion() {
    val delta = 0.01
    val l1 = Feuille(1.0 / 3.0, 'a') :: Feuille(1.0 / 3.0, 'b') :: Feuille(1.0 / 3.0, 'c') :: Nil
    val expected_1 = Noeud(
      1.0,
      Feuille(1.0 / 3.0, 'c'),
      Noeud(
        2.0 / 3.0,
        Feuille(1.0 / 3.0, 'a'),
        Feuille(1.0 / 3.0, 'b')))

    assertEqualsHuffman(expected_1, fusion(l1), delta)

    assertEqualsHuffman(Feuille(1.0, 'a'), fusion(Feuille(1.0, 'a') :: Nil), delta)
    assertEqualsHuffman(expected_1, fusion(expected_1 :: Nil), delta) // idempotence
  }

  // Pour écrire des tests unitaires qui comparent des Double,
  // il sera nécessaire d'utiliser l'assertion
  // def assertEquals(expected: Double, actual: Double, delta: Double): Unit
  // où le 3ème paramètre delta est la précision à utiliser.
  // On donne un exemple de test ci-dessous démontrant son utilisation.

  /**
   * Test qui illustre l'utilisation de l'assertion
   * assertEquals sur les Double
   */
  @Test
  def testEgaliteDouble() {
    // Exemple de problème d'imprécision sur les Double
    assertNotEquals(2.97, 2.8 + 0.17)

    // Problème résolu en utilisant une assertion spéciale à 3 paramètres Double
    assertEquals(2.97, 2.8 + 0.17, 0.01)

    // Selon la précision utilisée, deux Double sont soient égaux, soit différents
    assertEquals(0.001, 0.002, 0.001)
    assertNotEquals(0.001, 0.002, 0.0001)
  }

  // On vous fournit également une fonction auxiliaire permettant de
  // définir des tests pour contrôler l'"égalité" de deux arbres de Huffman.
  // Vous pouvez l'utiliser dans vos tests comme n'importe quelle autre assertion.

  /**
   * Vérifie que h1 et h2 sont égaux, en comparant les fréquences Double
   * avec la précision d. Echoue si ce n'est pas le cas.
   * @param h1 un arbre de Huffman
   * @param h2 un arbre de Huffman
   * @param d un double
   */
  def assertEqualsHuffman(h1: Huffman, h2: Huffman, d: Double): Unit = {
    (h1, h2) match {
      case (Feuille(f1, c1), Feuille(f2, c2)) => {
        assertEquals(f1, f2, d);
        assertEquals(c1, c2)
      }
      case (Noeud(f1, h11, h12), Noeud(f2, h21, h22)) =>
        assertEquals(f1, f2, d);
        assertEqualsHuffman(h11, h21, d);
        assertEqualsHuffman(h12, h22, d)
      case _ => fail("Les deux arbres n'ont pas la même structure")
    }
  }

  /**
   * Verifie que `expected` et `result` sont egales, en comparant les frequences
   * des Doubles des arbres de Huffman avec la precision d. Echoue si ce n'est pas le cas.
   * @param expected une liste d'arbre d'Huffman
   * @param result une liste d'arbre d'Huffman
   * @param d un double
   */
  def assertEqualsListHuffman(expected: List[Huffman], result: List[Huffman], d: Double): Unit = {
    (expected, result) match {
      case (Nil, Nil) => ()
      case (h1 :: t1, h2 :: t2) => {
        assertEqualsHuffman(h1, h2, d)
        assertEqualsListHuffman(t1, t2, d)
      }
      case _ => fail("Les deux listes n'ont pas le meme nombre d'elements")
    }
  }

  /**
   * Verifie que `expected` et `result` sont egales, en comparant les Doubles avec
   * la precision d. Echoue si ce n'est pas le cas.
   * @param expected une liste de tuple de Char et de Double
   * @param result une liste de tuple de Char et de Double
   * @param d un Double
   */
  def assertEqualsListCharDouble(expected: List[(Char, Double)], result: List[(Char, Double)], d: Double): Unit = {
    (expected, result) match {
      case (Nil, Nil) => ()
      case ((c1, f1) :: t1, (c2, f2) :: t2) => {
        assertEquals(c1, c2)
        assertEquals(f1, f2, d)
        assertEqualsListCharDouble(t1, t2, d)
      }
      case _ => fail("Les deux listes n'ont pas le meme nombre d'elements")
    }
  }

  /**
   * Test de codeHuffman()
   */
  @Test
  def testCodeHuffman() {
    val delta = 0.01
    val l1 = ('a', 1.0 / 3.0) :: ('b', 1.0 / 3.0) :: ('c', 1.0 / 3.0) :: Nil
    val expected_1 = Noeud(
      1.0,
      Feuille(1.0 / 3.0, 'c'),
      Noeud(
        2.0 / 3.0,
        Feuille(1.0 / 3.0, 'a'),
        Feuille(1.0 / 3.0, 'b')))

    assertEqualsHuffman(expected_1, codeHuffman(l1), delta)

    assertEqualsHuffman(Feuille(1.0, 'a'), codeHuffman(('a', 1.0) :: Nil), delta)
  }

  /**
   * Test de analyseFrequences()
   */
  @Test
  def testAnalyseFrequences() {
    val delta = 0.01

    assertEquals(Nil, analyseFrequences(""))
    assertEqualsListCharDouble(('a', 1.0) :: Nil, analyseFrequences("aaaaaaaa"), delta)
    assertEqualsListCharDouble(('a', 0.333) :: ('b', 0.333) :: ('c', 0.333) :: Nil, analyseFrequences("abcabcabc"), delta)
    assertEqualsListCharDouble(('1', 4.0 / 7.0) :: ('2', 2.0 / 7.0) :: ('3', 1.0 / 7.0) :: Nil, analyseFrequences("1111223"), delta)
  }

  /**
   * Test de descriptionHuffman()
   */
  @Test
  def testDescriptionHuffman() {
    assertEquals("0" + vers16Bits("a"), descriptionHuffman(Feuille(0.0, 'a')))

    val h1 = Noeud(
      1.00,
      Feuille(0.5, 'c'),
      Noeud(
        0.5,
        Feuille(0.25, 'a'),
        Feuille(0.25, 'b')))

    val expected_1 = "1" + "0" + vers16Bits("c") + "1" + "0" + vers16Bits("a") + "0" + vers16Bits("b")

    assertEquals(expected_1, descriptionHuffman(h1))

    val h2: Huffman = Noeud(
      1.00,
      Noeud(
        0.57,
        Feuille(0.25, 'a'),
        Noeud(
          0.32,
          Feuille(0.18, 'c'),
          Feuille(0.14, 'd'))),
      Noeud(
        0.43,
        Feuille(0.21, 'b'),
        Noeud(
          0.22,
          Noeud(
            0.13,
            Feuille(0.07, 'f'),
            Feuille(0.06, 'g')),
          Feuille(0.09, 'e'))))

    val expected_2 = "1" + "1" + "0" + vers16Bits("a") + "1" + "0" + vers16Bits("c") + "0" + vers16Bits("d") + "1" +
      "0" + vers16Bits("b") + "1" + "1" + "0" + vers16Bits("f") + "0" + vers16Bits("g") + "0" + vers16Bits("e")

    assertEquals(expected_2, descriptionHuffman(h2))
  }

  /**
   * Test de descriptionHuffman()
   */
  @Test
  def testEncode() {
    assertEquals(true, true)
  }

  /**
   * Test de lireDescription()
   */
  @Test
  def testLireDescription() {

  }

  /**
   * Test de la deuxieme implementation (surcharge) de decode()
   */
  @Test
  def testDecode2() {

  }
}
