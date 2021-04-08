package fr.istic.si2.huffman

import scala.io.StdIn
import Encodage._
import Decodage._
import Utils._
import ConstructionCode._

/**
 * Application principale V3 : avec transmission du code
 */
object HuffmanApp3 extends App {

  /**
   * @return si la boucle doit continuer
   */
  def mainLoop(): Boolean = {
    println("Fichier à encoder ? ")
    val filename_to_encode = StdIn.readLine()

    val file_content = lireFichier(filename_to_encode)
    val standard_encoding = vers16Bits(file_content)

    val lfreqs = analyseFrequences(file_content)

    val h: Huffman = codeHuffman(lfreqs)
    val description_huffman = descriptionHuffman(h)

    println("Voici la representation binaire de l'arbre de Huffman correspondant a ce fichier: " + description_huffman)

    val encoded = encode(file_content)

    val encoded_filename = filename_to_encode + "_compressed_v3"
    ecrireFichier(encoded_filename, encoded)
    println("Le fichier compressé a été écrit à " + encoded_filename)

    val decoded = decode(encoded)
    val decoded_filename = filename_to_encode + "_uncompressed_v3"
    ecrireFichier(decoded_filename, decoded)

    println("Le message décodé a été écrit à " + decoded_filename)

    println("Encore ? [Y/n]")
    val again_char = StdIn.readChar()

    (again_char match {
      case 'Y' | 'y' => true // actually does nothing...
      case _         => false
    })
  }

  while (mainLoop()) {} // Such do while, much wow
}