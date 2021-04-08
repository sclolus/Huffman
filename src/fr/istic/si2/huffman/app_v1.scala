package fr.istic.si2.huffman

import Encodage._
import scala.io.StdIn

import Decodage._
import ConstructionCode._
import Utils._

/**
 * Application principale V1 : arbre de code fixé
 */
object HuffmanApp1 extends App {

  /**
   * Arbre de code utilisé par l'application principale
   */
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

  /**
   * @return Si la boucle doit continuer
   */
  def mainLoop(): Boolean = {
    println("Chaîne à encoder ? ")
    val string_to_encode = StdIn.readLine()

    println("Chaîne encodée standard :")
    val encodage_standard = vers16Bits(string_to_encode)
    println("\t" + encodage_standard)
    println("\ttaille (nb Bits) : " + encodage_standard.length)

    val encodage_huffman = encodeList(string_to_encode.toList, h)
    println("Chaïne encodée Huffman: ")
    println("\t" + listBitToString(encodage_huffman))
    println("\ttaille (nb Bits): " + encodage_huffman.length)

    // TOPO FIX THIS
    val decoded_string = decode(encodage_huffman, h).getOrElse("")
    println("Chaîne décodée Huffman: ")
    println("\t" + decoded_string)

    if (string_to_encode.length != decoded_string.length) {
      println("Erreur ou caractère(s) non encodable(s)")
    }

    println("Encore ? [Y/n]")
    val again_char = StdIn.readChar()

    (again_char match {
      case 'Y' | 'y' => true
      case _         => false
    })
  }

  while (mainLoop()) {} // Emulation d'un do while...

}