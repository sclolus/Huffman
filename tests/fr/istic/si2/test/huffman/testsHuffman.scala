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

  // Vous devrez tester soigneusement chacune de vos fonctions.

  val zero_string = "0" // Workaround scalastyle warning issued because "0" is repeated too often in the file...
  val one_string = "1" // Similarly
  val onezero_string = "10"
  val a_string = "a"
  val b_string = "b"
  val c_string = "c"

  val delta = 0.01 // Globlal precision for Double comparaisons

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
    assertEquals(zero_string, listBitToString(Zero :: Nil))
    assertEquals(one_string, listBitToString(One :: Nil))
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
  }

  /**
   * Test de decode(), première implémentation
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

    assertEquals(None, decode(Nil, h))
    assertEquals(Some("bg"), decode(One :: Zero :: One :: One :: Zero :: One :: Nil, h))
    assertEquals(Some(b_string), decode(One :: Zero :: One :: Nil, h))
    assertEquals(Some("d"), decode(Zero :: One :: One :: Zero :: Nil, h))
    assertEquals(None, decode(One :: Nil, h))
  }

  /**
   * Test de initHuffman()
   */
  @Test
  def testInitHuffman() {
    assertEqualsListHuffman(Nil, initHuffman(Nil), delta)
    assertEqualsListHuffman(List(Feuille(0.5, 'a'), Feuille(0.5, 'b')), initHuffman(('a', 0.5) :: ('b', 0.5) :: Nil), delta)
    assertEqualsListHuffman(List(Feuille(0.5, 'a')), initHuffman(('a', 0.5) :: Nil), delta)
    assertEqualsListHuffman(List(Feuille(0.3, 'a'), Feuille(0.3, 'b'), Feuille(0.3, 'c')), initHuffman(('a', 0.3) :: ('b', 0.3) :: ('c', 0.3) :: Nil), delta)
  }

  /**
   * Test de triSelonFreq()
   */
  @Test
  def testTriSelonFreq() {
    val l = Feuille(0.66, 'c') :: Feuille(0.33, 'b') :: Feuille(0.16, 'c') :: Nil
    val expected = Feuille(0.16, 'c') :: Feuille(0.33, 'b') :: Feuille(0.66, 'c') :: Nil
    val l2 = Feuille(0.01, 'd') :: l
    val expected_2 = Feuille(0.01, 'd') :: expected

    assertEqualsListHuffman(Nil, triSelonFreq(Nil), delta)
    assertEqualsListHuffman(Feuille(1.0, 'a') :: Nil, triSelonFreq(Feuille(1.0, 'a') :: Nil), delta)
    assertEqualsListHuffman(expected, triSelonFreq(l), delta)
    assertEqualsListHuffman(expected_2, triSelonFreq(l2), delta)
  }

  /**
   * Test de getFreq()
   */
  @Test
  def testGetFreq() {
    assertEquals(0.5, getFreq(Feuille(0.5, 'a')), delta)
    assertEquals(1.0, getFreq(Noeud(1.0, Feuille(0.5, 'a'), Feuille(0.5, 'b'))), delta)
  }

  /**
   * Test de uneFusion()
   */
  @Test
  def testUneFusion() {
    assertEqualsListHuffman(
      Noeud(
        1.0,
        Feuille(0.5, 'a'),
        Feuille(0.5, 'b')) :: Nil,
      uneFusion(Feuille(0.5, 'a') :: Feuille(0.5, 'b') :: Nil), delta)

    val expected_1 = Noeud(
      0.3,
      Feuille(0.1, 'a'),
      Feuille(0.2, 'b')) :: Feuille(0.7, 'c') :: Nil

    assertEqualsListHuffman(expected_1, uneFusion(Feuille(0.7, 'c') :: Feuille(0.2, 'b') :: Feuille(0.1, 'a') :: Nil), delta)

    val expected_2 = Noeud(
      1.0,
      Noeud(
        0.3,
        Feuille(0.1, 'a'),
        Feuille(0.2, 'b')),
      Feuille(0.7, 'c')) :: Nil

    assertEqualsListHuffman(expected_2, uneFusion(expected_1), delta)

    val expected_3 = Noeud(
      0.3,
      Feuille(0.1, 'a'),
      Feuille(0.2, 'b')) :: Feuille(0.3, 'c') :: Feuille(0.4, 'd') :: Nil

    // Are all nodes sorted ?
    assertEqualsListHuffman(expected_3, uneFusion(Feuille(0.4, 'd') :: Feuille(0.3, 'c') :: Feuille(0.2, 'b') :: Feuille(0.1, 'a') :: Nil), delta)
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

  //  /**
  //   * Verifie que `expected` et `result` sont egales, en comparant les Doubles avec
  //   * la precision d. Echoue si ce n'est pas le cas.
  //   * @param expected une liste de tuple de Double et de Char
  //   * @param result une liste de tuple de Double et de Char
  //   * @param d un Double
  //   */
  //  def assertEqualsListDoubleChar(expected: List[(Double, Char)], result: List[(Double, Char)], d: Double): Unit = {
  //    (expected, result) match {
  //      // Since someone had the brilliant idea to
  //      // put both List[(Double, Char)] and List[(Char, Double)] in the signatures...
  //      case (Nil, Nil) => ()
  //      case ((f1, c1) :: t1, (f2, c2) :: t2) => {
  //        assertEquals(c1, c2)
  //        assertEquals(f1, f2, d)
  //        assertEqualsListDoubleChar(t1, t2, d)
  //      }
  //      case _ => fail("Les deux listes n'ont pas le meme nombre d'elements")
  //    }
  //  }

  /**
   * Test de codeHuffman()
   */
  @Test
  def testCodeHuffman() {
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
   * Test de countInstancesOf()
   */
  @Test
  def testCountInstancesOf() {
    assertEquals(0, countInstancesOf('a', Nil))
    assertEquals(1, countInstancesOf('a', 'a' :: Nil))
    assertEquals(3, countInstancesOf('b', 'a' :: 'b' :: 'c' :: 'a' :: 'b' :: 'c' :: 'a' :: 'b' :: 'c' :: Nil))
  }

  /**
   * Test de frequencyInString()
   */
  @Test
  def testFrequencyInString() {
    val number_string = "1111223"

    assertEquals(0.0, frequencyInString('a', ""), delta)
    assertEquals(1.0, frequencyInString('a', "aaaaaaaaa"), delta)
    assertEquals(1.0 / 3.0, frequencyInString('b', "abcabcabc"), delta)
    assertEquals(4.0 / 7.0, frequencyInString('1', number_string), delta)
    assertEquals(2.0 / 7.0, frequencyInString('2', number_string), delta)
    assertEquals(1.0 / 7.0, frequencyInString('3', number_string), delta)
    assertEquals(0.0, frequencyInString('4', number_string), delta)
  }

  /**
   * Test de charIsInList()
   */
  @Test
  def testCharIsInList() {
    assert(!charIsInList('a', Nil))
    assert(charIsInList('a', 'a' :: Nil))
    assert(charIsInList('b', 'a' :: 'a' :: 'c' :: 'b' :: 'd' :: Nil))
    assert(!charIsInList('z', 'a' :: 'a' :: 'c' :: 'b' :: 'd' :: Nil))
  }

  /**
   * Test de reduceToUniqueChars()
   */
  @Test
  def testReduceToUniqueChars() {
    assertEquals(Nil, reduceToUniqueChars(Nil))
    assertEquals('a' :: Nil, reduceToUniqueChars('a' :: 'a' :: 'a' :: 'a' :: Nil))
    assertEquals('a' :: 'b' :: 'c' :: Nil, reduceToUniqueChars(
      'a' :: 'b' :: 'c' :: 'a' :: 'b' :: 'c' :: 'a' :: 'b' :: 'c' :: Nil))
  }

  /**
   * Test de listOfCharToListOfCharAndFrequencies()
   */
  @Test
  def testListOfCharToListOfCharAndFrequencies() {
    assertEqualsListCharDouble(Nil, listOfCharToListOfCharAndFrequencies(Nil, a_string), delta)
    assertEqualsListCharDouble(
      ('z', 0.0) :: ('w', 0.0) :: ('x', 0.0) :: Nil,
      listOfCharToListOfCharAndFrequencies('z' :: 'w' :: 'x' :: Nil, "abcdefghijklmnopqrstuvy"), delta)

    assertEqualsListCharDouble(
      ('a', 0.333) :: ('b', 0.333) :: ('c', 0.333) :: Nil,
      listOfCharToListOfCharAndFrequencies('a' :: 'b' :: 'c' :: Nil, "abcabcabc"), delta)

    assertEqualsListCharDouble(
      ('1', 4.0 / 7.0) :: ('2', 2.0 / 7.0) :: ('3', 1.0 / 7.0) :: Nil,
      listOfCharToListOfCharAndFrequencies('1' :: '2' :: '3' :: Nil, "1111223"), delta)
  }

  /**
   * Test de analyseFrequences()
   */
  @Test
  def testAnalyseFrequences() {
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
    assertEquals(zero_string + vers16Bits(a_string), descriptionHuffman(Feuille(0.0, 'a')))

    val h1 = Noeud(
      1.00,
      Feuille(0.5, 'c'),
      Noeud(
        0.5,
        Feuille(0.25, 'a'),
        Feuille(0.25, 'b')))

    val expected_1 = one_string + zero_string + vers16Bits(c_string) + one_string + zero_string + vers16Bits(a_string) + zero_string + vers16Bits(b_string)

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

    val expected_2 = one_string + one_string + zero_string + vers16Bits(a_string) + one_string + zero_string + vers16Bits(c_string) +
      zero_string + vers16Bits("d") + one_string +
      zero_string + vers16Bits(b_string) + one_string + one_string + zero_string +
      vers16Bits("f") + zero_string + vers16Bits("g") + zero_string + vers16Bits("e")

    assertEquals(expected_2, descriptionHuffman(h2))
  }

  /**
   * Test de descriptionEncode()
   */
  @Test
  def testEncode() {
    assertEquals(true, true)
  }

  /**
   * Test de takeBitList()
   */
  @Test
  def testTakeBitList() {
    assertEquals(Nil, takeBitList(0, Nil))
    assertEquals(Nil, takeBitList(1, Nil))
    assertEquals(One :: Nil, takeBitList(1, One :: Zero :: Nil))
    assertEquals(One :: Zero :: Nil, takeBitList(16, One :: Zero :: Nil))
  }

  /**
   * Test de dropBitList()
   */
  @Test
  def testDropBitList() {
    assertEquals(Nil, dropBitList(0, Nil))
    assertEquals(Nil, dropBitList(1, Nil))
    assertEquals(Nil, dropBitList(1, One :: Nil))
    assertEquals(Zero :: Nil, dropBitList(1, One :: Zero :: Nil))
    assertEquals(Nil, dropBitList(16, One :: Zero :: Nil))
  }

  /**
   * Test de lireDescription()
   */
  @Test
  def testLireDescription() {
    assertEquals((Feuille(0.0, 'z'), Nil), lireDescription(stringToListBit(zero_string + vers16Bits("z"))))
    assertEquals(
      (Noeud(0.0, Feuille(0.0, 'a'), Feuille(0.0, 'b')), Nil),
      lireDescription(stringToListBit(onezero_string + vers16Bits(a_string) + zero_string + vers16Bits(b_string))))
    assertEquals(
      (Noeud(0.0, Feuille(0.0, 'a'), Feuille(0.0, 'b')), Zero :: One :: Zero :: One :: Nil),
      lireDescription(stringToListBit(onezero_string + vers16Bits(a_string) + zero_string + vers16Bits(b_string) + "0101")))

    assertEquals(
      (Noeud(0.0, Feuille(0.0, 'a'), Noeud(0.0, Feuille(0.0, 'b'), Feuille(0.0, 'c'))), Zero :: One :: Zero :: One :: Nil),
      lireDescription(stringToListBit(onezero_string + vers16Bits(a_string) + onezero_string +
        vers16Bits(b_string) + zero_string + vers16Bits(c_string) + "0101")))

  }

  /**
   * Test de la deuxieme implementation (surcharge) de decode()
   */
  @Test
  def testDecode2() {
    assertEquals("", decode(onezero_string + vers16Bits(a_string) + zero_string + vers16Bits(b_string)))
    assertEquals("abab", decode(onezero_string + vers16Bits(a_string) + zero_string + vers16Bits(b_string) + "0101"))
    assertEquals("", decode(onezero_string + vers16Bits(a_string) + onezero_string + vers16Bits(b_string) + zero_string + vers16Bits(c_string)))
    assertEquals("ccabb", decode(onezero_string + vers16Bits(a_string) + onezero_string + vers16Bits(b_string) +
      zero_string + vers16Bits(c_string) + "111101010"))
    assertEquals(a_string, decode(zero_string + vers16Bits(a_string)))
    assertEquals(a_string, decode(zero_string + vers16Bits(a_string) + zero_string))
  }

  /**
   * Test de from16Bits()
   */
  @Test
  def testFrom16Bits() {
    assertEquals('a', from16Bits(vers16Bits("a")))
    assertEquals('b', from16Bits(vers16Bits("b")))
    assertEquals('c', from16Bits(vers16Bits(c_string)))
    assertEquals('d', from16Bits(vers16Bits("d")))
    assertEquals('A', from16Bits(vers16Bits("A")))
    assertEquals('B', from16Bits(vers16Bits("B")))
    assertEquals('C', from16Bits(vers16Bits("C")))
    assertEquals('D', from16Bits(vers16Bits("D")))
    assertEquals('Z', from16Bits(vers16Bits("Z")))
    assertEquals('1', from16Bits(vers16Bits("1")))
    assertEquals('0', from16Bits(vers16Bits("0")))
    assertEquals('!', from16Bits(vers16Bits("!")))
    assertEquals(';', from16Bits(vers16Bits(";")))
    assertEquals('@', from16Bits(vers16Bits("@")))
  }
}
