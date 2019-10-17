package fr.polytech.sgit

import java.io.{File => fi}
import java.time.Instant
import fr.polytech.sgit.objects.Tools._
import fr.polytech.sgit.objects.wdFile
import better.files.File
import fr.polytech.sgit.objects.Repository
import fr.polytech.sgit.sgitParser.{Parser, ParserConfig}
import scopt.OParser

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
      println(config.command)
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
                case "diff" => //TODO
                case "commit" => println(srepository.commit(config.message))
                case "log" => //TODO
                case "branch" => //TODO
                case _ => {
                  val b = List("a")
                  println(b.tail == Nil)



                  val map =getTreeContent("fb6e148b92c9dbde5fa7fbd7bb9905448ab79fe9",Map.empty,srepository.path)
                  println(map + "\n \n")
                  val c = map.values.flatten
                  val m = c.map(blobLine => {val cm =blobLine.split("\t").tail
                    cm.head -> cm.last
                  }).toMap



                  println(m)




                  println("     Command needed..." )
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