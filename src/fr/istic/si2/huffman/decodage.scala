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
        (if (head == Zero) {
          decodeSymbol(zero, tail)
        } else {
          decodeSymbol(one, tail)
        })
      }
      case (Noeud(_, _, _), Nil) => (None, Nil)
    }
  }

  /**
   * @param l une liste de bits
   * @param h un arbre de Huffman
   * @return la chaîne correspondant au décodage de l, selon h, si elle existe.
   * Dans le cas où un caractere ne pourrait etre decode, la liste de bit
   * n'est pas assez grande par rapport au chemin pris dans l'arbre.
   * Bien que cette liste privee du premier bit pourrait etre suffisemment
   * grande pour donner un charactere decompresse (car elle pourrait prendre un
   * tout autre chemin), ce comportement ne pourrait de toute facon n'avoir lieux
   * qu'en fin de message, ce qui indique qu'il est plus coherent de simplement decider
   * ce suffix de bit indecodable comme superflu et erone.
   */
  def decode(l: List[Bit], h: Huffman): Option[String] = {
    (l, h) match {
      case (Nil, Feuille(_, _)) => None
      // In the special case of a single-leaf Huffman tree, such as Feuille(_, 'a'),
      // since the encoding of "a" leads to the empty list, interpreting decoding is tricky here:
      // An empty list of Bits might mean a string consisting of any number of 'a's.
      // However, it seems coherent that we would like to be able to encode any string
      // from the language defined by the characters of any given Huffman tree,
      // which would be impossible if we choosed to interpret decoding as such for the given scenario.
      // Hence, we decide that for a single-leaf Huffman tree, a List[Bit] of length `n` means the string
      // consisting of `n` times the character encodable in this tree.
      // Remark: We could have used the bits to encode the length of that string in binary form.
      // That would have been neater, but it's too much of an edge-case to invest time in it.
      case (_ :: tail, Feuille(_, char)) => Some(char.toString + (decode(tail, h) match {
        case Some(tail_string) => tail_string
        case None              => ""
      }))
      case (_, _) => {
        val (sym, tail) = decodeSymbol(h, l)

        val tail_string = tail match {
          case Nil    => None
          case _ :: _ => decode(tail, h)
        }

        (sym, tail_string) match {
          case (Some(c), Some(string)) => Some(c + string) // If there is both a decoded char and a decoded string, concatenate them
          case (Some(c), None)         => Some(c.toString) // If only the first char can be decoded take it
          case (None, _)               => None // But if neither can be decoded, no string is created
          // Note that (None, Some(_)) cannot actually happen since decodeSymbol always consumes the whole list of bits if a char cannot be decoded
        }
      }
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
        (Feuille(0.0, toChar(listBitToString(takeBitList(16, tail)))), dropBitList(16, tail))

      case One :: tail => {
        val (zero, leftover) = lireDescription(tail)
        val (one, leftleftover) = lireDescription(leftover)
        (Noeud(0.0, zero, one), leftleftover)
      }
      // Should never happen by specification.
      // But a non-exhaustive match would lead to a warning,
      // so we're throwing an error here.
      case Nil => sys.error("lireDescription was fed a none valid representation")
      // TODO: Check it one last time
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
      // A non-existent decoded message is converted to the null String.
      // This seems to be the better solution over returning some error string.
      case None          => ""
      case Some(decoded) => decoded
    }
  }
}