package fr.istic.si2.huffman

object ConstructionCode {

  /**
   * @param l une liste de couples caractère/fréquence
   * @return la liste des arbres de Huffman réduits à des feuilles,
   *         un pour chaque élément de l
   */
  def initHuffman(l: List[(Char, Double)]): List[Huffman] = {
    l match {
      case Nil               => Nil
      case (c, freq) :: tail => Feuille(freq, c) :: initHuffman(tail)
    }
  }

  /**
   * @param n un arbre d'Huffman
   * @param l une liste d'arbre d'Huffman triee dans l'ordre croissant par frequence aux racines
   * @return la liste `l` avec `n` inséré de telle manière à ce que la liste soit triée
   */
  def insertionHuffman(n: Huffman, l: List[Huffman]): List[Huffman] = {
    val f = n match {
      case Feuille(f, _)  => f
      case Noeud(f, _, _) => f
    }

    l match {
      case Nil => List(n)
      case Noeud(freq, one, zero) :: tail => {
        if (f > freq) {
          Noeud(freq, one, zero) :: insertionHuffman(n, tail)
        } else {
          n :: Noeud(freq, one, zero) :: tail
        }
      }
      case Feuille(freq, c) :: tail => {
        if (f > freq) {
          Feuille(freq, c) :: insertionHuffman(n, tail)
        } else {
          n :: Feuille(freq, c) :: tail
        }
      }
    }
  }

  /**
   * @param l une liste d'arbres de Huffman
   * @return la liste des éléments de l, classée par ordre croissant des fréquences aux racines
   */
  def triSelonFreq(l: List[Huffman]): List[Huffman] = {
    l match {
      case Nil          => Nil
      case last :: Nil  => List(last)
      case head :: tail => insertionHuffman(head, triSelonFreq(tail))
    }
  }

  /**
   * @param h un arbre d'Huffman
   * @return la frequence a la racine
   */
  def getFreq(h: Huffman): Double = {
    h match {
      case Feuille(f, _)  => f
      case Noeud(f, _, _) => f
    }
  }

  /**
   * @param l une liste d'arbres de Huffman, de longueur au moins 2
   * @return la liste obtenue après avoir fusionné les 2 arbres de l de fréquences minimales
   */
  def uneFusion(l: List[Huffman]): List[Huffman] = {
    l match {
      case first :: second :: tail => {
        val freq_first = getFreq(first)
        val freq_second = getFreq(second)

        Noeud(freq_first + freq_second, first, second) :: tail
      }
      case Nil          => List() // Shall never happen
      case head :: tail => List() // Shall never Happen
    }
  }

  /**
   * @param l une liste NON VIDE d'arbres de Huffman.
   * @return l'arbre de Huffman obtenu en fusionnant successivement,
   *         et 2 par 2, les arbres de l de fréquences minimales
   */
  def fusion(l: List[Huffman]): Huffman = {
    l match {
      case last :: Nil => last
      case _           => fusion(uneFusion(triSelonFreq(l)))
    }
  }

  /**
   * @param freqs une liste de couples caractère/fréquence
   * @return l'arbre de code de Huffman correspondant à freqs
   */
  def codeHuffman(freqs: List[(Char, Double)]): Huffman = {
    fusion(initHuffman(freqs))
  }

  /**
   * @param c un charactere
   * @param s une chaine de charactere
   * @return la frequence de `c` dans `s`
   */
  def frequencyInString(c: Char, s: String): Double = {
    s.toList.count((current_c) => c == current_c) / s.length
  }
  
  /**
   * @param s une chaîne de caractères
   * @return la liste des couples (caractère, fréquence d'apparition),
   *         calculée à partir de s. Chaque élément couple (c, f) est tel que
   *         c est un caractère apparaissant dans s, et f est sa fréquence
   *         d'apparition dans s.
   */
  def analyseFrequences(s: String): List[(Char, Double)] = {
    s.toList.map((c) => (c, frequencyInString(c, s)))
  }

}