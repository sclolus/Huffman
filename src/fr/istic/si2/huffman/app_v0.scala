package fr.istic.si2.huffman

import scala.io.StdIn
import Encodage._
import Decodage._
import Utils._

/**
 * Application principale V0 : arbre de code fixé, encodage/décodage de caractères
 */
object HuffmanApp0 extends App {
 // TODO: Does this demonstrate all the features ?
  
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
        println("" + char + " " + bit_list + " " + corresponding_string)

      }
      case None => println("Ce caractère ne peut être encodé.")
    }
  }
}