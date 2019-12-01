package fr.polytech.sgit

import java.io.{File => fi}
import java.time.Instant

import fr.polytech.sgit.objects.Tools._
import fr.polytech.sgit.objects.wdFile
import better.files.File
import fr.polytech.sgit.objects.Repository
import fr.polytech.sgit.sgitParser.{Parser, ParserConfig}
import scopt.OParser

import scala.annotation.tailrec
import scala.math.max

object sGit_main extends App {

  /* Here is the first step before a sgit command execution
    so we have to:
     1. Retrieve and parse "sgit command line" according to the configurations defined (in OParser/ParserConfig.scala).
     2. Check validity of the arguments.
     3. Then, check if we are (or not) in a sGit repository to execute (or not) the command.
   */

  val currentDir = System.getProperty("user.dir")
  val / = fi.separator


  OParser.parse(Parser.parser, args, ParserConfig()) match {

    case Some(config) => {
      config.command match {
        case "init" => println(Repository.initRepository(currentDir))
        case _ => {
          Repository.isSgitRepository(false, currentDir) match {
            case true => {
              val srepository_path = Repository.getRepositoryPath(currentDir)
              val srepository = Repository(srepository_path.get)
              config.command match {
                case "add" => println(srepository.add(config.files))
                case "status" => srepository.status(currentDir)
                case "diff" => srepository.diff()
                case "commit" => println(srepository.commit(config.message))
                case "log" => println(srepository.log(srepository.path,config.option))
                case "branch" =>{ if (config.av) println(srepository.listAllRefs())
                else println(srepository.createTagOrCommit(config.element,true,false))
                }
                case "tag" => println(srepository.createTagOrCommit(config.element,false,true))
                case _ => {println("     Command needed..." )


                  def LCSMatrix(oldFile: List[String], newFile: List[String]): Array[Array[Int]] = {
                    val matrix = Array.ofDim[Int](newFile.length + 1, oldFile.length + 1)
                    @tailrec
                    def buildLCSMatrix(matrix: Array[Array[Int]], oldFile: List[String], newFile: List[String], i: Int, j: Int): Array[Array[Int]] = {
                      if (i > newFile.length || j > oldFile.length)
                        matrix

                      else {
                        if (newFile(i - 1).equals(oldFile(j - 1)))
                          matrix(i)(j) = matrix(i - 1)(j - 1) + 1
                        else matrix(i)(j) = max(matrix(i)(j - 1), matrix(i - 1)(j))
                        if (j == oldFile.length)
                          buildLCSMatrix(matrix, oldFile, newFile, i + 1, 1)
                        else buildLCSMatrix(matrix, oldFile, newFile, i, j + 1)
                      }
                    }
                    buildLCSMatrix(matrix, oldFile, newFile, 1, 1)
                  }

                  def buildDiff(matrix: Array[Array[Int]], oldFile: List[String], newFile: List[String]): List[String] = {
                    @tailrec
                    def buildDiffRec(matrix: Array[Array[Int]], oldFile: List[String], newFile: List[String], i: Int, j: Int, result: List[String]): List[String] = {
                      if (i < 1 || j < 1)
                        result
                      else if (matrix(i)(j) == matrix(i)(j - 1))
                        buildDiffRec(matrix, oldFile, newFile, i, j - 1, ("-" +oldFile(j - 1)) +: result)
                      else if (matrix(i)(j) == matrix(i - 1)(j))
                        buildDiffRec(matrix, oldFile, newFile, i - 1, j, ("+" + newFile(i - 1)) +: result)
                      else
                        buildDiffRec(matrix, oldFile, newFile, i - 1, j - 1, oldFile(j - 1) +: result)
                    }
                    buildDiffRec(matrix, oldFile, newFile, newFile.length, oldFile.length, List[String]())
                  }

                  def getDiff(oldFile: List[String], newFile: List[String]): List[String] = {
                    if (oldFile.isEmpty) newFile.map("+ " + _)
                    else {
                      if (newFile.isEmpty) oldFile.map("- " + _)
                      else
                        buildDiff(LCSMatrix(oldFile, newFile), oldFile, newFile)
                    }
                  }

                  def printDiff(name: String,result : List[String]): Unit ={
                    val finalDiff = result.map( line => {
                      println("line true? "+line.startsWith("+"))
                      if (line.startsWith("+")) Console.GREEN + line
                      else if (line.startsWith("-")) Console.RED + line
                      else Console.WHITE + line
                    }).mkString("\n") + "\n" + Console.WHITE
                    println(Console.BLUE + "diff  --sgit  a/"+ name + "  b/"+ name +"\n\n"+ finalDiff + "\n")
                  }
                    def diff(): Unit = {

                    //1. Récupère les fichiers de l'index
                    //2. Récupère les fichiers du WD
                    val list_wdFiles = getAllFiles("/Users/anissalamh/TESTLAPIN")
                      println("wdFiles ===     "+ list_wdFiles)
                    val index= "/Users/anissalamh/TESTLAPIN"+ "/.sgit/index"
                    //3. On récupère les fichiers modifiés

                    val modifiedOnes= getUntrackedOrModified(list_wdFiles, index).last;
                      println(" hhhhh"+ modifiedOnes)
                    if (modifiedOnes.nonEmpty) {
                      modifiedOnes.map(wdFile => {
                        val newContent = wdFile.lines.toList
                        println("new content  ==    "+ newContent );

                        val sameFileInStage= File(index).lines.toList.filter(line => line.contains(wdFile.pathAsString)).head;
                        println(sameFileInStage)

                        val BlobHash = sameFileInStage.split("\t").head
                        println("curr /Users/anissalamh/TESTLAPIN/")
                        val path= "/Users/anissalamh/TESTLAPIN/"
                        val BlobName = File(path).relativize(File(sameFileInStage.split("\t").last)).toString
                        println(BlobName)

                        val BlobPath ="/Users/anissalamh/TESTLAPIN/.sgit/objects/Blobs/"+ BlobHash
                        val oldContent= File(BlobPath).lines.toList
                        println(oldContent)
                        println("diff -----   "+ printDiff(BlobName,getDiff(oldContent,newContent)));
                        //  printer.printSingleDiff(x.path, diffHelper.diff(oldContent, newContent))*/
                      })

                    }
                      println("là ici là bas ");
                  }

                println("RES START ==============> \n" + diff() + "\n\n");
                  println("RES END -------------------------");

                // println("MATRIX : "+ '\n'+ buildMatrix(List("a","b","c"),List("a","c")).map(_.mkString).mkString("\n"));

                }

                }
              }
            case _ => println("Failed: You are not in a sgit repository")

          }
        }
      }
    }
    case None => println("Invalid command, Usage: sgit <command> [Option] <args>")
  }



}