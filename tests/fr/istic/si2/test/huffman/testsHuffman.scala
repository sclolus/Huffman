package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.testerApp._
import fr.istic.si2.huffman._
import fr.istic.si2.huffman.Utils._
import fr.istic.si2.huffman.ConstructionCode._

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

  }

  /**
   * Test de uneFusion()
   */
  @Test
  def testUneFusion() {

  }

  /**
   * Test de fusion()
   */
  @Test
  def testFusion() {

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
   * Test de codeHuffman()
   */
  @Test
  def TestCodeHuffman() {

  }

  /**
   * Test de analyseFrequences()
   */
  @Test
  def TestAnalyseFrequences() {

  }

  /**
   * Test de descriptionHuffman()
   */
  @Test
  def testDescriptionHuffman() {

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
  def TestLireDescription() {

  }

  /**
   * Test de la deuxieme implementation (surcharge) de decode()
   */
  @Test
  def testDecode2() {

  }
}
