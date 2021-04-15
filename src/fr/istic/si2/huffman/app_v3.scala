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
    val footer = "======================================="
    
    
    println("Fichier à encoder ? ")
    val filename_to_encode = StdIn.readLine()

    val file_content = lireFichier(filename_to_encode)
    val standard_encoding = vers16Bits(file_content)

    println("========== Contenu du fichier =========")
    println(file_content)
    println(footer)
    println("===== Contenu en encodage standard ====")
    println(standard_encoding)
    println(footer)

    val lfreqs = analyseFrequences(file_content)

    val h: Huffman = codeHuffman(lfreqs)

    println("Arbre de Huffman correspondant au contenu du fichier: ")
    println(h)
    println(footer)

    val description_huffman = descriptionHuffman(h)

    println("Voici la representation binaire de l'arbre de Huffman: ")
    println(description_huffman)
    println(footer)

    val encoded = encode(file_content)

    println("========== Contenu compressé ==========")
    println(encoded)
    println(footer)

    val encoded_filename = filename_to_encode + "_compressed_v3"
    ecrireFichier(encoded_filename, encoded)
    println("Le fichier compressé a été écrit à " + encoded_filename)

    val (read_description, rest) = lireDescription(stringToListBit(encoded))

    println("Representation de l'arbre lue depuis le message compressé")
    println(read_description)
    println(footer)
    println("== Le message restant à décompressé ===")
    println(listBitToString(rest))
    println(footer)

    val decoded = decode(encoded)
    val decoded_filename = filename_to_encode + "_uncompressed_v3"

    println("========== Contenu décompressé ==========")
    println(decoded)
    println(footer)
    ecrireFichier(decoded_filename, decoded)

    println("Le fichier décompressé a été écrit à " + decoded_filename)

    println("Encore ? [Y/n]")
    val again_char = StdIn.readChar()

    (again_char match {
      case 'Y' | 'y' => true
      case _         => false
    })
  }

  while (mainLoop()) {} // Such do while, much wow
}