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

    // Commit tree traversal results
    val commit_treeMap = prepareCommit(indexLines)

    // CreateTreeContent(commit_treeMap,path)
    // Information
    val commit_content= commit_treeMap.get("COMMIT_TREE").get.head
    val commit_sha = createHash(commit_content.split("\t").tail.head)
    val commitPath = path + / + ".sgit" + / + "objects" + / + "Commits" + / + commit_sha

    createTreeContent(commit_treeMap,path)
    // If log file doesnt exists => It's the first commit
    val isfirstcommit = !File(path + / + ".sgit" + / + "log").exists

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
      val commit_lines = commit_content + "\n" + "parents : " + "\t" + s"$commit_parent" + "\n"
      val commit_file = File(commitPath).createIfNotExists().writeText(commit_lines)
      val logFile = File(path + / + ".sgit" + / + "log").createIfNotExists(false)
      logFile.appendLine("Commit: "+commit_sha + "\t" + currentBranch + "\n Date: "+ instant + "\n" + "\t\t" +message +"\n\n")

      val master_branch = File(path + / + ".sgit" + / + "refs" + / + "headers" + / + currentBranch)
      master_branch.createIfNotExists(false).writeText(commit_sha)

      s"All files commited, Nothing to commit on $currentBranch"
    }
    else s"Nothing to commit on $currentBranch"
  }


  def prepareCommit(ftc: List[String]): Map[String, List[String]] = {
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

    buildIndex_tree(paths, Map(), deep, mapReturn, List(None))

  }

  var parents = Map.empty[String, List[String]]

  def buildIndex_tree(indexLines: List[String], childrenList: Map[String, List[String]], deep: Int, mapReturn: Map[String, String], lastContent: List[Option[String]]): Map[String, List[String]] = {

    // Stop condition: We are at the top of the repository (=> we have retrieve the main tree)
    if (deep == 0) {
      val commitContent = lastContent.head.getOrElse("no result").mkString("")
      addChildrenToParent("COMMIT_TREE",commitContent)
      parents
    }

    else {

      // We take all the index's paths and split them
      val splittedPaths = indexLines.map(p => {
        p.split(/).toList
      }).distinct

      //We filter the paths with the max length
      val pathsWeKeep = splittedPaths.filter(sp => sp.length == deep)

      // We build the path again
      val files = pathsWeKeep.map(pwk => pwk mkString (/))
      val blobs = files.filterNot(f => {
        File(repo.parent + / + f).isDirectory
      })
      val dirs = files.filter(f => File(repo.parent + / + f).isDirectory)
      val allFiles = List.concat(blobs, dirs).map(l => l.split(/).toList)

      if (childrenList.isEmpty) {

        val parentMapChildren = getChildrenByParent(pathsList = allFiles, mapReturn)

        val indexLinesUpdated = updateIndexPaths(indexLines, deep).distinct

        buildIndex_tree(indexLinesUpdated, parentMapChildren, deep - 1, mapReturn,List(None))

      }
      else {
        if (deep ==1) println("All files ==="+ allFiles)
        val parentMapChildren = getChildrenByParent(pathsList = allFiles, mapReturn)
        val lastTree=dirs.map(dir => {
          val dirName = dir.split(/).last

          val children = childrenList.filter(c => c._1 == dirName)
          val contentTree = children.head._2.distinct.mkString("\n")
          val dirLine = "tree" + "\t" + createHash(contentTree) + "\t" + dir

          val splittedDir = dir.split(/).toList
          val parentDropped = splittedDir.dropRight(1)

          if (parentDropped.nonEmpty) {addChildrenToParent(parentDropped.last, dirLine)
            None}
          else Some(dirLine)
        })
        val indexLinesUpdated = updateIndexPaths(indexLines, deep).distinct
        buildIndex_tree(indexLinesUpdated, parents, deep - 1, mapReturn,lastTree)
      }
    }
  }


  def getChildrenByParent(pathsList: List[List[String]], map: Map[String, String]): Map[String, List[String]] = {

    val parent = pathsList.map(f = pwk => {
      val parentPath = pwk.filterNot(a => a.contains(pwk.last))
      if(parentPath.nonEmpty){
      val parentDir = parentPath.last
      if (!File(repo.parent + / + pwk.mkString(/)).isDirectory) {
        val lineToAdd = "blob" + "\t" + map(pwk.mkString(/)) + "\t" + pwk.mkString(/)
        addChildrenToParent(parentDir, lineToAdd)
      }
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
    val index = stage

  val untrackedOrModified= getUntrackedOrModified(list_wdFiles, index.pathAsString)
    if (stage.exists){

      
         // firstSubList= newfile not committed, SecondSubList= file commited but modified, ThirdSubList= deleted but still present in last commit
          val uncommittedChanges = getUncommittedChanges(index,path)
          println(uncommittedChanges.mkString("\n\n &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&" +
            ""))
          if(uncommittedChanges.flatten.nonEmpty){
            println("Changes to be committed:"+"\n\n"+   uncommittedChanges.head.map(m=> "\tnew file:   " + File(currentPath).relativize(File(m))).mkString("\n") +"\n" + "\n" + uncommittedChanges.last.map(m=> "\tmodified:   " +  File(currentPath).relativize(File(m))).mkString("\n")+ "\n\n" + uncommittedChanges.tail.head.map(m=>"\t deleted:   " +  File(currentPath).relativize(File(m))).mkString("\n"))
          }


      val modified_unstagedOnes = untrackedOrModified.last
      val deleted_unstagedOnes = getDeletedUnstaged(list_wdFiles, index)
      if(untrackedOrModified.isEmpty && deleted_unstagedOnes.isEmpty){
        println("Your branch is up to date \nnothing to commit, working tree clean")
      }
      else if (modified_unstagedOnes.nonEmpty || deleted_unstagedOnes.nonEmpty) {
        println("Changes not staged for commit:\n  (use \"sgit add <file>...\" to update what will be committed)\n\n "+ modified_unstagedOnes.map(m => "\t modified:   " + File(currentPath).relativize(m) + "\n").mkString("")+  deleted_unstagedOnes.map(d => "\n \t deleted:   " + File(currentPath).relativize(File(d)) ).mkString("") +"\n")

      }}

    val untrackedOnes = untrackedOrModified.head
    if (untrackedOnes.nonEmpty) {
      println("Untracked files:\n " + "(use \"sgit add <file>...\" to include in what will be committed)\n\n" + untrackedOnes.map(u => "\t"+ File(currentPath).relativize(u)+ "\n").mkString(""))
      }
    else if (!stage.exists) println("\t No staged or commited files yet \n")


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


  def createTagOrCommit(refName: String,  branch: Boolean, tag: Boolean): String= {

    def isBranchOrTag(branch:Boolean,tag:Boolean):String = {if (branch) "headers"
    else "tags"}

    if (!File(path+ / + ".sgit" + / + "log").exists) "Not able to create branches or tags if no existing commits "
    else{
      val refType = isBranchOrTag(branch,tag)
      val refFile = File(path+ / + ".sgit"+ / + "refs"+ / + refType + / +refName)
      if (refFile.exists){
        "This name is already associated to an existing branch or tag"}
      else {
        refFile.createIfNotExists(false)
        val lastCommit = getLastCommit(path,getCurrentHead(path))
        refFile.appendLine(lastCommit)
        s"$refName successfully created"
      }}

    }


  def listAllRefs(): String= {

    val refsContent = getAllRefs(path)
    if (refsContent.isEmpty) "No references existing yet"
    else {
      val names = refsContent.map(r => {
        val currentRef = getCurrentHead(path)
        val nameRef = r.pathAsString.split(/).last
        val typeRef = r.pathAsString.split(/).dropRight(1).last
        val lineToPrint = typeRef match {
          case "headers" => "- " + nameRef + ": branch"
          case _ => "- " + nameRef + ": tag"
        }
        if (nameRef == currentRef) lineToPrint + s" * (HEAD -> $currentRef) "
        else lineToPrint
      })
      names.mkString("\n")
    }
  }
    /*--------------- checkout command functions----------------*/

    def checkout(ref: String): Unit = {


    }


}

object Repository{

  /*---------------init command functions----------------*/

  /* Initialize the sgit repository if it's not already done*/
  def initRepository(path:String): String = {
    if (!isSgitRepository(true, path)) {
      createRepository()
      "Initialised empty sGit repository in " + path
    }
    else "Unable to create a repository here: "+ path +", it's already a sgit repository"
  }

  /* Create the whole sgit folder tree structure */
  def createRepository() : Unit = {
    createFileOrDir(".sgit"+ / +"objects" + / + "Commits", true, true)
    createFileOrDir(".sgit"+ / +"objects" + / + "Trees", true, false)
    createFileOrDir(".sgit"+ / +"objects" + / + "Blobs", true, false)
    createFileOrDir(".sgit"+ / +"refs" + / +"tags", true, true)
    createFileOrDir(".sgit" + / + "refs"+ / + "headers", true, false)
    File(".sgit"+ / +"HEAD").createIfNotExists(false,false).appendLine("ref: refs/heads/master\n")
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
    val possible_sgitpath = File(s"$pathToTest${/}.sgit")
    val currentDirectory = new f(pathToTest)
    val found = possible_sgitpath.exists()

    // If init=true that means that we have just to check the current directory,
    // else we have to check the whole parent tree structure
    val parentDirectory = currentDirectory.getParent
    if(!found && !(parentDirectory == null)){
      getRepositoryPath(parentDirectory)
    }
    else if(found) Some(currentDirectory.getCanonicalPath)
    else None
  }
}


