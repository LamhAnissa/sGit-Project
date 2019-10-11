package fr.polytech.sgit
import java.io
import java.io.File

import better.files.File
import sgitParser._
import scopt.OParser

import scala.io.Source._
import fr.polytech.sgit.objects.Repository


object sGit_main extends App {

  /* Here is the first step before a sgit command execution
    so we have to:
     1. Retrieve and parse "sgit command line" according to the configurations defined (in OParser/ParserConfig.scala).
     2. Check validity of the arguments.
     3. Then, check if we are (or not) in a sGit repository to execute (or not) the command.
   */

  val currentDir = System.getProperty("user.dir")

  OParser.parse(Parser.parser, args, ParserConfig()) match {

    case Some(config) => {
      val srepository = Repository(currentDir)
      println(" je suis dans le Oparser")
      println(config.command)
      config.command match {
        case "init" => srepository.initRepository()
        case _ => {
          srepository.isSgitRepository(false,srepository.path) match {
            case true => {
              config.command match {
                case "add" => //TODO
                case "status" => //TODO
                case "diff" => //TODO
                case "commit" => //TODO
                case "log" => //TODO
                case "branch" => //TODO
                case _ => println("     Command needed...")

                }
              }
            }}
            case _ => println("Failed: You are not in a sgit repository")
          }

        }
      }
      } case None => println("Invalid command, Usage: sgit <command> [Option] <args>")
    }
}







