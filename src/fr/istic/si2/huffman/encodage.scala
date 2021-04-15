package fr.istic.si2.huffman

import Utils._
import ConstructionCode._

object Encodage {

  /**
   * @param c un caractère
   * @param h un arbre de Huffman
   * @return l'encodage de c, selon h (si c est bien présent dans h)
   */
  def encodeSymbol(c: Char, h: Huffman): Option[List[Bit]] = {
    h match {
      case Feuille(_, ce) => if (ce == c) { Some(List()) } else { None }
      case Noeud(_, zero, one) => {
        val ret_zero = encodeSymbol(c, zero)

        ret_zero match {
          case Some(list) => Some(Zero :: list)
          case None => {
            val ret_one = encodeSymbol(c, one)

            ret_one match {
              case Some(list) => Some(One :: list)
              case None       => None
            }
          }
        }
      }
    }
        //    h match {
    //      case Feuille(_, ce) => if (ce == c) { Some(List()) } else { None }
    //      case Noeud(_, zero, one) => encodeSymbol(c, zero)
    //           .map(list => Zero :: list)
    //           .orElse(encodeSymbol(c, one).map(list => One :: list))
    //    }
  }

  /**
   * @param l une liste de caractères
   * @param h un arbre de Huffman
   * @return la séquence de bits correspondants à
   *         l'encodage selon h des éléments de l, s'il a réussi.
   *         Les caractères pour lesquels l'encodage est impossible sont oubliés
   */
  def encodeList(l: List[Char], h: Huffman): List[Bit] = {
    l match {
      case Nil          => List()
      case head :: tail => (encodeSymbol(head, h) match {
        case None => Nil // Simply ignore unencodable characters
        case Some(l) => l
      }) ++ encodeList(tail, h)
    }
  }

  /**
   * @param s une chaîne de caractères
   * @param h un arbre de Huffman
   * @return l'encodage de s, selon h, en une liste de bits.
   *         (concaténation de l'encodage de chaque caractère de s selon h)
   */
  def encode(s: String, h: Huffman): List[Bit] = {
    encodeList(s.toList, h)
  }

  /**
   * @param h un arbre de Huffman
   * @return une chaîne de 0 et 1 uniquement représentant l'arbre h (voir partie 1.3 de l'énoncé)
   *         Les caractères encodables avec h sont représentés dans leur encodage binaire 16 bits.
   */
  def descriptionHuffman(h: Huffman): String = {
    h match {
      case Feuille(_, c)       => "0" + vers16Bits(c.toString)
      case Noeud(_, zero, one) => "1" + descriptionHuffman(zero) + descriptionHuffman(one)
    }
  }

  /**
   * @param message une chaîne de caractères
   * @return la chaîne de 0 et 1, contenant:
   *         - la représentation de l'arbre de huffman construit à partir de message
   *         - puis l'encodage de message en utilisant l'arbre construit à partir de message
   */
  def encode(message: String): String = {
    val arbre = codeHuffman(analyseFrequences(message))
      descriptionHuffman(arbre) + listBitToString(encode(message, arbre))
  }
}