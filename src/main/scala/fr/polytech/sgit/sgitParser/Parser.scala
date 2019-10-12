package fr.polytech.sgit.sgitParser

import scopt.OParser

object Parser {
  val builder = OParser.builder[ParserConfig]
  val parser = {
    import builder._
    OParser.sequence(
      programName("fr/polytech/sgit"),
      head("fr/polytech/sgit"),
      help("help").text("Review of all sgit commands usages "),

      // Create

      cmd("init")
        .action((_, c) => c.copy(command = "init"))
        .text("Create the sGit repository"),

      // Local Changes

      cmd("status")
        .action((_, c) => c.copy(command = "status"))
        .text("Show the working directory status"),

      cmd("diff")
        .action((_, c) => c.copy(command = "diff"))
        .text("Show changes between commits, commit and working directory, etc"),

      cmd("add")
        .action((_, c) => c.copy(command = "add"))
        .text("Add file contents to the index")
        .children(
          arg[String]("<file-name.txt> or blop")
            .unbounded()
            .action((x, c) => c.copy(files = c.files :+ x))
            .text("add 0.* files or blops")
        ),

      cmd("commit")
        .action((_, c) => c.copy(command = "commit"))
        .text("Record changes to the repository"),

      // Commit History

      cmd("log")
        .action((_, c) => c.copy(command = "log"))
        .text("Show commit logs")
        .children(
          opt[Unit]("p")
            .action((_, c) => c.copy(option = "p"))
            .text("Show changes overtime"),
          opt[Unit]("stat")
            .action((_, c) => c.copy(option = "stat"))
            .text("Show stats about changes overtime")
        ),

       // Branches and Tags

      cmd("branch")
        .action((_, c) => c.copy(command = "branch"))
        .text("Create a new branch")
        .children(
          arg[String]("<branch name>")
            .required()
            .action((x, c) => c.copy(element = x))
            .text("name of the branch we want to create"),
          opt[Unit]("a")
            .action((_, c) => c.copy(option = "a"))
            .text("List all branches"),
          opt[Unit]("v")
            .action((_, c) => c.copy(option = "v"))
            .text("List for each head: hash and subject")
        ),

      cmd("checkout")
        .action((_, c) => c.copy(command = "checkout"))
        .text("Switch to a branch")
        .children(
          arg[String]("<Branch or tag or commit hash>")
            .required()
            .action((x, c) => c.copy(element = x))
            .text(" ")
        ),

      cmd("tag")
        .action((_, c) => c.copy(command = "tag"))
        .text("Create a new tag")
        .children(
          arg[String]("<tag name>")
            .required()
            .action((x, c) => c.copy(element = x))
            .text("name of the tag we want to create")
        ),

      // Merge and Rebase

      cmd("merge")
        .action((_, c) => c.copy(command = "merge"))
        .text("Merge a branch into the active branch")
        .children(
          arg[String]("<branch>")
            .required()
            .action((x, c) => c.copy(element = x))
            .text("Branch with which we want to merge ")
        ),

      cmd("rebase")
        .action((_, c) => c.copy(command = "rebase"))
        .text("Reapply commits on top of another base tip")
        .children(
          arg[String]("<branch>")
            .required()
            .action((x, c) => c.copy(element = x))
            .text("  "),
          opt[String]("i")
            .action((x, c) => c.copy(option = "i"))
            .text("Set the mode of the rebase to interactive")
        ),
    )
  }
}
