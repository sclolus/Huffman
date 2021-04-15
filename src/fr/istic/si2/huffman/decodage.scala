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
        val (sym, new_tail) = (if (head == Zero) {
          decodeSymbol(zero, tail)
        } else {
          decodeSymbol(one, tail)
        })

        sym match {
          case None    => (None, tail)
          // In case the character cannot be decoded,
          // we return the list of bits with the first bit removed
          // instead of consuming the whole list.
          // TODO: Do we really want this ? Seems likely that an already too short list
          // remains too short, although not certain
          case Some(_) => (sym, new_tail)
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
  // TODO: Justifier les choix
  def decode(l: List[Bit], h: Huffman): Option[String] = {
    val (sym, tail) = decodeSymbol(h, l)
//
  //  println(l + " decoded: " + sym + " tail: " + tail)
    val tail_string = tail match {
      case Nil    => None
      case _ :: _ => decode(tail, h)
    }
   // println(tail_string)

    (sym, tail_string) match {
      case (Some(c), Some(string)) => Some(c + string)
      case (Some(c), None)         => Some(c.toString) // If only the first char can be decoded take it
      case (None, Some(string))    => Some(string) // Or if the first char cannot be decoded but the rest can, take the rest
      case (None, None)            => None // But if neither can be decoded, no string is created
    }
  }

  /**
   * @param n un entier positif ou nul
   * @param l une liste de Bit
   * @return La liste constituée des `n` premiers éléments de `l` au maximum
   */
  def takeBitList(n: Int, l: List[Bit]): List[Bit] = {
    l match {
      case Nil => Nil
      case head :: tail => if (n != 0) {
        head :: takeBitList(n - 1, tail)
      } else {
        Nil // early return.
      }
    }
  }

  /**
   * @param n un entier positif ou nul
   * @param l une liste de Bit
   * @return La liste `l` privée de ces `n`, au maximum, premiers éléments
   */
  def dropBitList(n: Int, l: List[Bit]): List[Bit] = {
      l match {
        case Nil => Nil
        case head :: tail => if (n != 0) { 
            dropBitList(n - 1, tail)
          } else {
            head :: dropBitList(0, tail)
          }
      }
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
        (Feuille(0.0, from16Bits(listBitToString(takeBitList(16, tail)))), dropBitList(16, tail))

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

    decode(message, arbre) match {
      case None          => ""
      case Some(decoded) => decoded
    }
  }
}