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
    triSelonFreq(l) match { // Which means uneFusion also sorts.
      case first :: second :: tail => {
        val freq_first = getFreq(first)
        val freq_second = getFreq(second)

        Noeud(freq_first + freq_second, first, second) :: tail
      }
      case Nil          => List() // Shall never happen by specs
      case head :: tail => List() // Shall never Happen by specs
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
      case _           => fusion(uneFusion(l))
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
   * @param c un Char
   * @param list une liste de Char
   * @return le nombre d'occurence de `c` dans `list`
   */
  def countInstancesOf(c: Char, list: List[Char]): Int = {
    list match {
      case Nil          => 0
      case head :: tail => (if (c == head) { 1 } else { 0 }) + countInstancesOf(c, tail)
      // Note that the parenthesis are actually necessary here since the compiler's parser
      // would interpret ```... { 0 } + countInstancesOf(c, tail)``` as
      // ``` { 0 + countInstancesOf(c, tail) } ``` which would not be
      // the meaning intended.
    }
  }

  /**
   * @param c un charactere
   * @param s une chaine de charactere
   * @return la frequence de `c` dans `s`
   */
  def frequencyInString(c: Char, s: String): Double = {
    val count = countInstancesOf(c, s.toList)
    
    if (s.length == 0) {
      0.0 // Let's avoid floating point division by zero
          // The frequency of any char in a null String is zero anyway.
    } else {
      count.asInstanceOf[Double] / s.length.asInstanceOf[Double]
    }
  }

  /**
   * @param char un Char
   * @param list une liste de Char
   * @return si `char` se trouve dans `list`
   */
  def charIsInList(char: Char, list: List[Char]): Boolean = {
    list match {
      case Nil          => false
      case head :: tail => char == head || charIsInList(char, tail)
    }
  }

  /**
   * @param chars une liste de Char
   * @return la liste `chars` où tous les caractères apparaissent
   * une unique fois dans l'ordre original
   */
  def reduceToUniqueChars(chars: List[Char]): List[Char] = {
    def reduce(chars: List[Char], acc: List[Char]): List[Char] = {
      chars match {
        case Nil => acc
        case head :: tail => if (charIsInList(head, acc)) { // Gotta love n^2 complexity huh...
          reduce(tail, acc)
        } else {
          reduce(tail, acc ++ List(head))
        }
      }
    }
    reduce(chars, Nil)
  }

  /**
   * @param l une liste de Char
   * @param reference_string une String
   * @return La liste `l` transformée en une liste ayant
   * pour chaque caractère sa fréquence associée dans la String `reference_string`
   */
  def listOfCharToListOfCharAndFrequencies(l: List[Char], reference_string: String): List[(Char, Double)] = {
    // Naming is getting out of hand... Blame the absence of higher-order fonctions
    // lambdas and generics ; Leading to inelegant and semantically redundant code.

    l match {
      case Nil => Nil
      case head :: tail => (head, frequencyInString(head, reference_string)) ::
        listOfCharToListOfCharAndFrequencies(tail, reference_string)
    }
  }

  /**
   * @param s une chaîne de caractères
   * @return la liste des couples (caractère, fréquence d'apparition),
   *         calculée à partir de s. Chaque élément couple (c, f) est tel que
   *         c est un caractère apparaissant dans s, et f est sa fréquence
   *         d'apparition dans s.
   */
  def analyseFrequences(s: String): List[(Char, Double)] = {
    listOfCharToListOfCharAndFrequencies(reduceToUniqueChars(s.toList), s)
  }

}