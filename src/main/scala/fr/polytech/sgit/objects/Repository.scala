package fr.polytech.sgit.objects

import better.files._
import java.io.{FileWriter, File => f}
import java.time.Instant

import wdFile._
import Tools._

case class Repository(val path: String) {


  val / = f.separator
  val stage = File(path + s"${/}.sgit${/}index")


  /*-----------add command functions----------------*/

  //Finished
  def add(files: Seq[String]): String = {

    val listCheckedFiles = checkFiles(files, List.empty, List.empty)
    if (listCheckedFiles.last.isEmpty) {
      addFilesToStage(listCheckedFiles.head)
      "Success files have been added"
    }
    else {
      val tostring = listCheckedFiles.last.mkString(", ")
      " The following file(s): " + tostring + " could not be added cause did not match any files"
    }

  }

  def checkFiles(files: Seq[String], validNames: List[String], fails: List[String]): List[List[String]] = {

    if (files == Nil) List(validNames, fails)
    else {val file = files.head
    if (File(file).exists) {
      val listOk = file :: validNames
      checkFiles(files.tail, listOk, fails)
    }
    else {
      val listNotOk = file :: fails
      checkFiles(files.tail, validNames, listNotOk)
    }
  }}

  //Finished
  def addFilesToStage(filesToBeStaged: List[String]): Unit = {

    if (!stage.exists) {
      stage.createFile()
      filesToBeStaged.map(f => {
        val fileContent = File(f).contentAsString
        val sha = createHash(fileContent)
        stage.appendLine(s"$sha\t${File(f).canonicalPath}")
      })
    }
    else filesToBeStaged.map(f => UpdateIfNeeded(f))
  }

  //Finished
  def UpdateIfNeeded(fileToTest: String): Unit = {

    val file = File(fileToTest)
    val sha = createHash(file.contentAsString)
    val filepath = file.canonicalPath
    val wdfile = wdFile(sha, filepath)
    val res = getState(wdfile, stage)

    if (!(res == "Staged")) {
      if (res == "Modified") {
        val newcontent = stage.lines.toList.filterNot(l => l.contains(filepath))
        val filewriter = new FileWriter(stage.canonicalPath)
        filewriter.write(newcontent.mkString("\n") + "\n")
        filewriter.close()
      }
      stage.appendLine(s"$sha\t$filepath")
    }
  }

  /*-----------commit command functions----------------*/
  val repo = File(path)


  def commit(message: String): String = {

    //Stage lines
    val indexLines = File(path + "/.sgit/index").lines.toList

    // Commit information
    val commit_content = prepareCommit(indexLines)
    val commit_sha = createHash(commit_content)
    val commitPath = path + / + ".sgit" + / + "objects" + / + "Commits" + / + commit_sha

    // If master branch doesnt exists => It's the first commit
    val isfirstcommit = !File(path + / + ".sgit" + / + "refs" + / + "headers" + / + "master").exists

    println("Commit content    :   " + commit_content)

    var commit_parent = ""
    val currentBranch = getCurrentHead(path)
    println(currentBranch)
    var todo = true

    if (!isfirstcommit) {
      // Commit up to date
      if (commit_sha == getLastCommit(path, currentBranch)) {
        todo = false
      }
      else {
        commit_parent = getLastCommit(path, currentBranch)
      }
    }
    // New Commit to add (case first commit or different commit)
    if (todo) {
      val instant = Instant.now().toString
      val commit_lines = commit_content + "\n" + "parents : " + "\t" + s"$commit_parent" + "\n" + "Date" + "\t" + Instant.now().toString
      val commit_file = File(commitPath).createIfNotExists().writeText(commit_lines)
      val logFile = File(path + ".sgit" + "log").createIfNotExists()
      logFile.appendLine("Commit : "+commit_sha + "\t" + currentBranch + "\n Date: "+ instant + "\n\n" )

      File(path + / + ".sgit" + / + "refs" + / + "headers" + / + currentBranch).writeText(commit_sha)
      val master_branch = File(path + / + ".sgit" + / + "refs" + / + "headers" + / + currentBranch)
      if (!master_branch.exists) {
        master_branch.createIfNotExists().appendLine(commit_sha)
      }
      s"All files commited, Nothing to commit on $currentBranch"
    }
    else s"Nothing to commit on $currentBranch"

  }


  def prepareCommit(ftc: List[String]): String = {
    val parentRepoPath = repo.parent.toString()

    // Retrieve files(blobs) related information
    val shas = ftc.map(s => s.split("\t")(0))
    val paths = ftc.map(s => {
      val absolutePath = s.split("\t").last
      absolutePath.split(parentRepoPath).last.tail

    })
    val mapReturn = (paths zip shas).toMap
    val splittedPaths = paths.map(p => {
      p.split(/).toList
    })
    val lengthMap = splittedPaths.map(pb => pb.length)
    val deep = lengthMap.max

    val commit_treeContent = buildIndex_tree(paths, Map(), deep, mapReturn)
    commit_treeContent.toString

  }

  var parents = Map.empty[String, List[String]]

  def buildIndex_tree(indexLines: List[String], childrenList: Map[String, List[String]], deep: Int, mapReturn: Map[String, String], lastContent: String = ""): Unit = {

    if (deep == 0 && lastContent != "") {
      val tree_nameFile = createHash(lastContent)
      val treePath = repo + / + ".sgit" + / + "objects" + / + "Trees" + / + tree_nameFile
      val commit_treeFile = File(treePath).createIfNotExists(false).appendLine(lastContent)
      val commitContent = "tree" + "\t" + createHash(lastContent) + "\t"
      commitContent
    }
    else {
      println("-----------------Tours-------------\n")


      println("-----------------Etape avec deep = " + deep + "-------------\n \n")
      val splittedPaths = indexLines.map(p => {
        p.split(/).toList
      }).distinct
      /*        println(" splittedPaths : === " + splittedPaths)
              println("\n ----------------------------------------\n")*/
      val pathsWeKeep = splittedPaths.filter(sp => sp.length == deep)
      println(" Les chemins splittés qu'on garde : ===== " + pathsWeKeep)
      println("\n -----------------------------------------\n")
      val files = pathsWeKeep.map(pwk => pwk mkString (/))
      println("   Les chemins qu'on garde : ====" + files)
      println("\n ------------------------\n")
      val blobs = files.filterNot(f => {
        File(repo.parent + / + f).isDirectory
      })
      val dirs = files.filter(f => File(repo.parent + / + f).isDirectory)
      val allFiles = List.concat(blobs, dirs).map(l => l.split(/).toList)

      if (childrenList.isEmpty) {
        val parentListChildren = getChildrenByParent(pathsList = allFiles, mapReturn)
        println("\n ------------------------\n")
        println(" La liste d'enfants par parents : ======" + parentListChildren)
        println("")
        val indexLinesUpdated = updateIndexPaths(indexLines, deep).distinct
        println("    La nouvelle liste qu'on obtient: =====" + indexLinesUpdated)
        println("\n ------------------------\n")
        println("")
        buildIndex_tree(indexLinesUpdated, parentListChildren, deep - 1, mapReturn)

      }
      else {
        dirs.foreach(dir => {
          val dirName = dir.split(/).last
          val children = childrenList.filter(c => c._1 == dirName)

          val blobchildren = children.head._2.filter(c => c.contains("blob")).distinct
          val blobObjectPath = (repo + / + ".sgit" + / + "objects" + / + "Blobs" + /)
          blobchildren.foreach(bc => {
            val blobName = bc.mkString("").split("\t").last
            val blobPath = blobObjectPath + mapReturn(blobName)
            val newBlob = File(blobPath)
            val filePath = File(repo.parent + / + blobName)
            if (!newBlob.exists) filePath.copyTo(newBlob)
          })
          val dirchildren = children.head._2.filter(c => c.contains("tree")).distinct
          println("\n ------------------------\n")
          println(" Les enfants  : Type Blob")
          println(blobchildren)
          println("-----------")
          println(" Tous les enfants : ")
          println(children.head._2)
          println("\n ------------------------\n")


          val contentTree = children.head._2.distinct.mkString("\n")
          println("----------Content tree avant création du tree------------")
          println(contentTree)
          println("------------------------------------------------")
          println(" Content Tree :  ============" + contentTree)

          val splittedDir = dir.split(/).toList

          val dirLine = "tree" + "\t" + createHash(contentTree) + "\t" + splittedDir.mkString(/)
          val parentDropped = splittedDir.dropRight(1)
          if (!parentDropped.isEmpty) {
            addChildrenToParent(parentDropped.last, dirLine)

            println("\n ------------------------\n")
            println(" splitted dir    =====" + splittedDir)


            val dirPath = (repo + / + ".sgit" + / + "objects" + / + "Trees" + / + createHash(contentTree))
            val newFile = File(dirPath).createIfNotExists(false).appendLine(contentTree)

            println("\n ------------------------\n")
            println(" blob children 2    " + "\n" + contentTree)
            val parentListChildren = getChildrenByParent(pathsList = allFiles, mapReturn)
            println("\n ------------------------\n")
            println(" parentListChildren : ======" + parentListChildren + "\n")
            println("\n ------------------------\n")
          }
          val indexLinesUpdated = updateIndexPaths(indexLines, deep).distinct
          println(" indexLinesUpdated : =====" + indexLinesUpdated)
          println("\n ------------------------\n")
          buildIndex_tree(indexLinesUpdated, parents, deep - 1, mapReturn, contentTree)

        })
      }

    }
  }


  def getChildrenByParent(pathsList: List[List[String]], map: Map[String, String]): Map[String, List[String]] = {

    val parent = pathsList.map(f = pwk => {
      val parentPath = pwk.filterNot(a => a.contains(pwk.last))
      val parentDir = parentPath.last
      if (!File(repo.parent + / + pwk.mkString(/)).isDirectory) {
        val lineToAdd = "blob" + "\t" + map(pwk.mkString(/)) + "\t" + pwk.mkString(/)
        addChildrenToParent(parentDir, lineToAdd)
      }
    })
    parents

  }

  def addChildrenToParent(key: String, value: String): Unit = {
    parents += (key -> (value :: (parents get key getOrElse Nil)))
  }


  /*-----------status command functions----------------*/

  //change le retour en string
  def status(currentPath: String): Unit = {

    val list_wdFiles = getAllFiles(path)
    val index = File(path + / + ".sgit" + / + "index")
    // firstSubList= newfile not committed, SecondSubList= file commited but modified, ThirdSubList= deleted but still present in last commit
    val uncommittedChanges = getUncommittedChanges(index,path)
    if(uncommittedChanges.nonEmpty){
      println("Changes to be committed:"+"\n\n"+   uncommittedChanges.head.map("\tnew file:   " + _).mkString("\n") +"\n" + "\n" + uncommittedChanges.last.map("\tmodified:   " + _).mkString("\n")+ "\n\n" + uncommittedChanges.tail.head.map("\t deleted:   " + _).mkString("\n"))
    }

    val modified_unstagedOnes = getUntrackedOrModified(list_wdFiles, index).last
    val deleted_unstagedOnes = getDeletedUnstaged(list_wdFiles, index)
    if (modified_unstagedOnes.nonEmpty || deleted_unstagedOnes.nonEmpty) {
     println("Changes not staged for commit:\n  (use \"sgit add <file>...\" to update what will be committed)\n\n "+ modified_unstagedOnes.map(m => "\t modified:   " + File(currentPath).relativize(m) + "\n").mkString("")+  deleted_unstagedOnes.map(d => "\n \t deleted:   " + File(currentPath).relativize(File(d))).mkString("") +"\n")
    }

    val untrackedOnes = getUntrackedOrModified(list_wdFiles, index).head
    if (untrackedOnes.nonEmpty) {
      println("Untracked files:\n " + "(use \"sgit add <file>...\" to include in what will be committed)\n\n" + untrackedOnes.map(u => "\t"+ File(currentPath).relativize(u)+ "\n").mkString(""))
    }
    if(!stage.exists) println("On branch master\n\nNo commits yet\n\n")
    else if(uncommittedChanges.isEmpty && list_wdFiles.diff(index.lines.toList).isEmpty){
      println("Your branch is up to date \nnothing to commit, working tree clean")
    }
  }
  /*---------------log command functions----------------*/

  def log(repoPath: String): Unit = {
    val logFile = File(repoPath + / + ".sgit" + / + "log" )
    if (logFile.exists){
      val commitsResume = logFile.contentAsString.split("\n\n")
      commitsResume.mkString(" ----------------------------------------")
    }
  }

  /*--------------- branch, tag command functions----------------*/


  /*def branch(repoPath: String, branchName: String): String = {
    val branchFile = File(repoPath+ / + ".sgit"+ / + "refs"+ / + "headers"+ / +branchName)
    if (branchFile.exists){
      "This name is already associated to an existing branch"
      else {
        branchFile.createIfNotExists(false)
        val lastCommit = getLastCommit(repoPath,getCurrentHead(repoPath))
        branchFile.appendLine(lastCommit)
        s"$branchName has been created"
      }
    }
    }*/

  def createTagOrCommit(refName: String,  branch: Boolean, tag: Boolean): String= {

    def isBranchOrTag(branch:Boolean,tag:Boolean):String = {if (branch) "headers"
    else "tags"}

    val refType = isBranchOrTag(branch,tag)
    val refFile = File(path+ / + ".sgit"+ / + "refs"+ / + refType + / +refName)
    if (refFile.exists){
      "This name is already associated to an existing branch or tag"
      else {
        refFile.createIfNotExists(false)
        val lastCommit = getLastCommit(path,getCurrentHead(path))
        refFile.appendLine(lastCommit)
        s"$refName has been created"
      }
    }
  }
}

object Repository{

  /*---------------init command functions----------------*/

  /* Initialize the sgit repository if it's not already done*/
  def initRepository(path:String): String = {
    if (!isSgitRepository(true, path)) {
      createRepository()
      "Initialised empty sGit repository in " + path + / +".sgit"
    }
    else "Unable to create a repository here:" + path + / +"sgit already existing"
  }

  /* Create the whole sgit folder tree structure */
  def createRepository() : Unit = {
    createFileOrDir(".sgit"+ / +"objects" + / + "Commits", true, true)
    createFileOrDir(".sgit"+ / +"objects" + / + "Trees", true, false)
    createFileOrDir(".sgit"+ / +"objects" + / + "Blobs", true, false)
    createFileOrDir(".sgit"+ / +"refs" + / +"tags", true, true)
    createFileOrDir(".sgit" + / + "refs"+ / + "headers", true, false)
    ".sgit"+ / +"HEAD".toFile.createIfNotExists(false,false).appendLine("ref: refs/heads/master\n")
  }

  /* Check if we are in a sgit repository. Look for the .sgit in current directory, if we haven't returned,
  recurse in parent
   */
  def isSgitRepository(init: Boolean, pathToTest: String): Boolean = {
    init match {
      case true => File(s"$pathToTest${/}.sgit").exists
      case _ => getRepositoryPath(pathToTest).isDefined
    }
  }

  /* Return the sgit repository path by looking reccursively in the parent's folder
   */
  def getRepositoryPath(pathToTest: String): Option[String] = {
    val possible_sgitpath = new f(s"$pathToTest${/}.sgit")
    val currentDirectory = new f(pathToTest)
    val found = possible_sgitpath.exists()

    // If init=true that means that we have just to check the current directory,
    // else we have to check the whole tree structure
    val parentDirectory = currentDirectory.getParent
    if(!found && !(parentDirectory == null)){
      getRepositoryPath(parentDirectory)
    }
    else if(found) Some(currentDirectory.getCanonicalPath)
    else None
  }
}


