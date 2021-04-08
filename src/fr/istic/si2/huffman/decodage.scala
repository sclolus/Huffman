package fr.istic.si2.huffman

import Utils._

object Decodage {

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return caractère correspondant au décodage de l selon h
   *          si l est un chemin valide de h
   */
  def decodeSymbolv0(h: Huffman, l: List[Bit]): Option[Char] = {
    (h, l) match {
      case (Feuille(_, c), Nil) => Some(c)
      case (Noeud(_, zero, one), head :: tail) => {
        if (head == Zero) {
          decodeSymbolv0(zero, tail)
        } else {
          decodeSymbolv0(one, tail)
        }
      }
      case _ => None
    }
  }

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return un tuple de taille 2
   *         - première composante : caractère correspondant au décodage selon h d'un préfixe de l
   *         - deuxième composante : la liste des bits restant à décoder
   */
  def decodeSymbol(h: Huffman, l: List[Bit]): (Option[Char], List[Bit]) = {
    (h, l) match {
      case (Feuille(_, c), list) => (Some(c), list)
      case (Noeud(_, zero, one), head :: tail) => {
        if (head == Zero) {
          decodeSymbol(zero, tail)
        } else {
          decodeSymbol(one, tail)
        }
      }
      case (Noeud(_, _, _), Nil) => (None, Nil)
    }
  }

  /**
   * @param l une liste de bits
   * @param h un arbre de Huffman
   * @return la chaîne correspondant au décodage de l, selon h, si elle existe
   */
  // TODO: fix this: should have none case
  def decode(l: List[Bit], h: Huffman): Option[String] = {
    val (sym, tail) = decodeSymbol(h, l)

    sym.map(c => c.toString).map(head => decode(tail, h).map(ttail => head + ttail)).flatten
    
//    Some(sym.map(c => "" + c).getOrElse("") + (tail match {
//      case Nil  => ""
//      case t @ _ => decode(t, h).getOrElse("")
//    }))
//    tail match {
//      case Nil => None
//      case t @ _ => {
//        sym.map(c => c.ToString)
//      }
//    }
  }

  /**
   * @param l une liste de bits décrivant, au moins, la représentation binaire d'un arbre de Huffman
   * @return un tuple de taille 2 comprenant :
   *         - l'arbre de code de Huffman reconstruit à partir du début de l
   *         - le reste de la liste l, après la représentation de l'arbre de Huffman
   */
  def lireDescription(l: List[Bit]): (Huffman, List[Bit]) = {
    l match {
      case Zero :: tail =>
        (Feuille(0.0, from16Bits(listBitToString(tail.take(16)))), tail.drop(16))

      case One :: tail => {
        val (zero, leftover) = lireDescription(tail)
        val (one, leftleftover) = lireDescription(leftover)
        (Noeud(0.0, zero, one), leftleftover)
      }
      case Nil => sys.error("lireDescription was fed a none valid representation") // Should never happen by specification
    }
  }

  /**
   * @param messageEnc une chaîne de 0 et 1 uniquement, contenant la représentation
   *                   d'un arbre de Huffman, puis un message encodé
   * @return le message décodé contenu dans messageEnc, en utilisant le code de huffman
   *         représenté en début de messageEnc
   */
  def decode(messageEnc: String): String = {
    val (arbre, message) = lireDescription(stringToListBit(messageEnc))
    
    decode(message, arbre).getOrElse("")
  }

}