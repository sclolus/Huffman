package fr.istic.si2.huffman

import Encodage._
import Decodage._
import ConstructionCode._
import Utils._
import scala.io.StdIn

/**
 * Application principale V2 : avec construction du code
 */
object HuffmanApp2 extends App {

  //  /**
  //   * Une liste de couples caractère / fréquence d'apparition
  //   * à utiliser par l'application principale.
  //   */
  //  val lfreqs: List[(Char, Double)] = List(
  //    ('a', 0.25),
  //    ('c', 0.18),
  //    ('d', 0.14),
  //    ('b', 0.21),
  //    ('f', 0.07),
  //    ('g', 0.06),
  //    ('e', 0.09))
  //
  //  val h: Huffman = codeHuffman(lfreqs)

  /**
   * @return si la boucle doit continuer
   */
  def mainLoop(): Boolean = {
    println("Fichier à encoder ? ")
    val filename_to_encode = StdIn.readLine()

    val file_content = lireFichier(filename_to_encode)
    val standard_encoding = vers16Bits(file_content)

    val lfreqs = analyseFrequences(file_content)

    println(lfreqs)
    val h: Huffman = codeHuffman(lfreqs)

    val encoded: List[Bit] = encode(file_content, h)
    val encoded_string = listBitToString(encoded)

    val encoded_filename = filename_to_encode + "_compressed"
    ecrireFichier(encoded_filename, encoded_string)
    println("Le fichier compressé a été écrit à " + encoded_filename)

    // Le taux de compression est défini ainsi: τ = [Volume final] / [Volume initial]
    val taux: Double = encoded_string.length.asInstanceOf[Double] / standard_encoding.length.asInstanceOf[Double] * 100.0

    println("Le taux de compression est de: " + taux + "%")

    println("Encore ? [Y/n]")
    val again_char = StdIn.readChar()

    (again_char match {
      case 'Y' | 'y' => true // actually does nothing...
      case _         => false
    })
  }

  while (mainLoop()) {} // Do while
}