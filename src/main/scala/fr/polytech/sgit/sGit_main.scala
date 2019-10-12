package fr.polytech.sgit

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

  OParser.parse(Parser.parser, args, ParserConfig()) match {

    case Some(config) => {
      val srepository = Repository(currentDir)
      println(config.command)
      config.command match {
        case "init" => println(srepository.initRepository())
        case _ => {
          srepository.isSgitRepository(false,srepository.path) match {
            case true => {
              config.command match {
                case "add" => println(srepository.add(config.files))
                case "status" => //TODO
                case "diff" => //TODO
                case "commit" => //TODO
                case "log" => //TODO
                case "branch" => //TODO
                case _ => println("     Command needed...")

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
