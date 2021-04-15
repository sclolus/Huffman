package fr.istic.si2.huffman

import scala.io.StdIn
import Encodage._
import Decodage._
import Utils._

/**
 * Application principale V0 : arbre de code fixé, encodage/décodage de caractères
 */
object HuffmanApp0 extends App {
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

  while (42 == 42) {
    print("Veuillez entrer un caractère à encodé: ")
    val char: Char = StdIn.readChar()

    encodeSymbol(char, h) match {
      case Some(bit_list) => {
        val corresponding_string = listBitToString(bit_list)
        val decoded_char = decodeSymbolv0(h, bit_list)

        println("" + char + " " + bit_list + " " + corresponding_string + " " + (decoded_char match {
          case Some(char) => char
          case None       => "Le charactère n'a pas pu être décodé" // Should never happen...
        }))
      }
      case None => println("Ce caractère ne peut être encodé.")
    }
  }
}
