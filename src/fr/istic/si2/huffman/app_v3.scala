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

  val footer = "======================================="

  /**
   * Affiche les étapes de la compression du fichier au nom `filename`
   * @param filename une String
   */
  def encoding(filename: String): Unit = {
    val file_content = lireFichier(filename)
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

    val encoded_filename = filename + "_compressed_v3"
    ecrireFichier(encoded_filename, encoded)
    println("Le fichier compressé a été écrit à " + encoded_filename)
  }

  /**
   * @param list une liste de Char
   * @return si la liste `list` est bien composée seulement de 0 et de 1
   */
  def isCharListBitList(list: List[Char]): Boolean = {
    list match {
      case Nil          => true
      case head :: tail => (head == '1' || head == '0') && isCharListBitList(tail)
    }
  }

  /**
   * Affiche les étapes de la décompression du fichier au nom `filename`
   * @param filename une String
   */
  def decoding(filename: String): Unit = {
    val encoded = lireFichier(filename)

    println("=== Contenu du fichier compressé ======")
    println(encoded)
    println(footer)

    if (!isCharListBitList(encoded.toList)) {
      println("Ce fichier ne contient pas que des 0 et des 1. Ce fichier n'est pas un fichier compressé. Il ne peut être décompressé")
    } else {

      val (read_description, rest) = lireDescription(stringToListBit(encoded))

      println("Representation de l'arbre lue depuis le message compressé")
      println(read_description)
      println(footer)
      println("== Le message restant à décompressé ===")
      println(listBitToString(rest))
      println(footer)

      val decoded = decode(encoded)
      val decoded_filename = filename + "_uncompressed_v3"

      println("======== Contenu décompressé ==========")
      println(decoded)
      println(footer)
      ecrireFichier(decoded_filename, decoded)

      println("Le fichier décompressé a été écrit à " + decoded_filename)
    }
  }

  sealed trait Mode
  case object Compression extends Mode
  case object Decompression extends Mode

  /**
   * Prompt l'utilisateur pour un choix de mode et lit ce choix sur l'entrée standarde 
   * @return le mode sélectionné par l'utilisateur
   */
  def choixMode(): Mode = {
    println("Voulez-vous compresser ou décompresser le fichier ? [c/d]")
    val choice_char = StdIn.readChar()

    choice_char match {
      case 'C' | 'c' => Compression
      case 'D' | 'd' => Decompression
      case _ => {
        println("Choix incorrect. Veuillez recommencer.")
        choixMode()
      }
    }
  }

  /**
   * La boucle principale prompt l'utilisateur et lit ces réponses sur l'entrée standarde.
   * Les étapes de compression/décompressions sont affichées.
   * @return si la boucle doit continuer
   */
  def mainLoop(): Boolean = {

    println("Fichier à compresser/décompresser ? ")
    val filename = StdIn.readLine()

    val mode = choixMode()

    mode match {
      case Compression   => encoding(filename)
      case Decompression => decoding(filename)
    }

    println("Encore ? [Y/n]")
    val again_char = StdIn.readChar()

    (again_char match {
      case 'Y' | 'y' => true
      case _         => false
    })
  }

  while (mainLoop()) {} // Such do while, much wow

}