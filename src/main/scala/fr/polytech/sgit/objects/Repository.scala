package fr.polytech.sgit.objects

import better.files._
import java.io.{FileWriter, File => f}
import java.util.Calendar

import wdFile._
import Tools._

import scala.annotation.tailrec
import scala.math.max

case class Repository(val path: String) {


  val / = f.separator
  val stage = File(path + s"${/}.sgit${/}index")


  /*-----------add command functions----------------*/

  /**
   *
   * @param files : files we want to add
   * @return Success or Failed message for add process
   */
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

  /**
   *
   * @param files: files we want to check the validity before adding them
   * @param validNames: files found we keep
   * @param fails: files not founded so rejected
   * @return List of valid files for the add process, and files rejected of the add process
   */
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

  /**
   *
   * @param filesToBeStaged:  files to add to the index file
   */
  def addFilesToStage(filesToBeStaged: List[String]): Unit = {

    val dirs= filesToBeStaged.filter(f=> File(f).isDirectory)
    val recFiles= dirs.flatMap(dir => getAllFiles(dir))
    val textFiles= filesToBeStaged.filterNot(f=> File(f).isDirectory)
    val allFiles= recFiles.concat(textFiles.map(t=> File(t)))
    if (!stage.exists) {
      stage.createFile()
        allFiles.map(tf => {
          val fileContent = tf.contentAsString
         val sha = createHash(fileContent)
         stage.appendLine(s"$sha\t${tf.canonicalPath}")})
    }
     else allFiles.map(f => UpdateIfNeeded(f.pathAsString))
  }


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
        if (newcontent!= Nil){
          filewriter.write(newcontent.mkString("\n") + "\n")
        }
        else filewriter.write( "")
        filewriter.close()
      }
      stage.appendLine(s"$sha\t$filepath")
    }
  }

  /*-----------commit command functions----------------*/
  val repo = File(path)

  /**
   *
   * @param message:  message to save with the commit
   * @return: Success or Failed message for commit process
   */
  def commit(message: String): String = {

    //Stage lines
    val indexLines = File(path + "/.sgit/index").lines.toList

    // Commit tree traversal results
    val commit_treeMap = prepareCommit(indexLines)

    // Information
    val commit_content= commit_treeMap.get("COMMIT_TREE").get.head
    val commit_sha = createHash(commit_content.split("\t").tail.head)
    val commitPath = path + / + ".sgit" + / + "objects" + / + "Commits" + / + commit_sha

    createTreeContent(commit_treeMap,path)
    // If log file doesnt exists => It's the first commit
    val isfirstcommit = !File(path + / + ".sgit" + / + "log").exists

    var commit_parent = " "
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
      val instant = Calendar.getInstance.getTime
      val commit_lines = commit_content + "\n" + "parents : " + "\t" + s"$commit_parent" + "\n" + "message : "+ "\t"+ message + "\n" +  "Date : "+ "\t" + instant
      val commit_file = File(commitPath).createIfNotExists().writeText(commit_lines)
      val logFile = File(path + / + ".sgit" + / + "log").createIfNotExists(false)
      logFile.appendLine("commit "+commit_sha + "\t" + "(HEAD -> " + currentBranch + ")" + "\n Date: "+ instant + "\n" + "\t\t" +message +"\b")

      val master_branch = File(path + / + ".sgit" + / + "refs" + / + "headers" + / + currentBranch)
      master_branch.createIfNotExists(false).writeText(commit_sha)

      s"All files commited, Nothing to commit on $currentBranch"
    }
    else s"Nothing to commit on $currentBranch"
  }

  /**
   *
   * @param ftc:  file to commit
   * @return map corresponding to the index tree (parents -> children)
   */
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

  /**
   *
   * @param indexLines:  List of the index lines content
   * @param childrenList: Map with the parent and children(trees or blobs)
   * @param deep: the level of the node we are
   * @param mapReturn: map with the initial index content
   * @param lastContent: the content of the commit to return
   * @return a map with reccursive sons of the commit tree
   */
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

  /**
   *
   * @param pathsList: the path from which we want to retrieve the parents
   * @param map: previous map with parent children that we want to update
   * @return new map up to date with the new paths is parents
   */
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

  /**
   *
   * @param key the parent
   * @param value the childrens
   */
  def addChildrenToParent(key: String, value: String): Unit = {
    parents += (key -> (value :: (parents get key getOrElse Nil)))
  }


  /*-----------status command functions----------------*/

  /**
   *
   * @param currentPath: path from where we called this method
   */
  def status(currentPath: String): Unit = {
    val list_wdFiles = getAllFiles(path)
    val branch = getCurrentHead(path)
    println( s"On branch $branch  \n")
    if (getLastCommit(path, branch)== "") println("No commits yet \n")
  val untrackedOrModified= getUntrackedOrModified(list_wdFiles, stage.pathAsString)


    if (stage.exists){


         // firstSubList= newfile not committed, SecondSubList= file commited but modified, ThirdSubList= deleted but still present in last commit
          val uncommittedChanges = getUncommittedChanges(stage,path)
          if(uncommittedChanges.flatten.nonEmpty){
            println("Changes to be committed:"+"\n\n"+ Console.GREEN + uncommittedChanges.head.map(m=> "\tnew file:   " + File(currentPath).relativize(File(m))).mkString("\n") + Console.WHITE + "\n" + uncommittedChanges.tail.head.map(m=> Console.GREEN +  "\tmodified:   " +  File(currentPath).relativize(File(m))).mkString("\n")+ Console.WHITE+ "\n\n" )
          }


      val modified_unstagedOnes = untrackedOrModified.last

      if(untrackedOrModified.flatten.isEmpty && uncommittedChanges.flatten.isEmpty ){
        println("Your branch is up to date \nnothing to commit, working tree clean")
      }
      else if (modified_unstagedOnes.nonEmpty) {
        println("Changes not staged for commit:\n  (use \"sgit add <file>...\" to update what will be committed)\n\n "+ modified_unstagedOnes.map(m => Console.GREEN +"\t modified:   " + File(currentPath).relativize(m) + "\n").mkString("")+ Console.WHITE +"\n")

      }}

    val untrackedOnes = untrackedOrModified.head
    if (untrackedOnes.nonEmpty) {
      println("Untracked files:\n " + "(use \"sgit add <file>...\" to include in what will be committed)\n\n" + Console.RED + untrackedOnes.map(u => "\t"+ File(currentPath).relativize(u)+ "\n").mkString("") +Console.WHITE)
      }
    else if (!stage.exists) println("\t No staged or commited files yet \n")


  }
  /*---------------log command functions----------------*/

  def log(repoPath: String, option: String): Unit = {

    val currentBranch = getCurrentHead(path)
    val logFile = File(repoPath + / + ".sgit" + / + "log" )

    if (logFile.exists){
      if (option!="p"){
        val commitsResume = logFile.contentAsString.split("\b").mkString("\n\n")
        println(commitsResume)
      }
      else logOptionP(currentBranch)
    }
   else  {

      println(s"fatal: your current branch $currentBranch does not have any commits yet")}
  }

  def logOptionP(currentBranch: String): Unit = {

    //1. Si il n'y a qu'un seul commit (commit.parent not exist)
    val lastCommit = getLastCommit(path,currentBranch)
    val lastParentCommit = getParentCommit(path, lastCommit)

    logOptionPRecc(lastCommit, lastParentCommit)
  }

  def logOptionPRecc( commit:String, parentCommit:String): Unit ={

    val commitContent = getContentFromCommit(path,commit)

    if(parentCommit == " "){
      printCommitLines(path,commit)
      commitContent.foreach(line => {
        val filename = line.split("\t").last
        println(Console.BLUE + s"diff --sgit a/$filename  b/$filename  "+ "\n newfile \n" + Console.WHITE)
      })
    }
else{ val parentContent = getContentFromCommit(path,parentCommit)
      val newFiles = commitContent.filter(line => parentContent.contains(line))
      val modifiedFiles = commitContent.filterNot(line=> newFiles.contains(line))
      printCommitLines(path,commit)
      if (newFiles.nonEmpty){
        newFiles.map(line => {
          val splitted = line.split("\t")
          val blobsha= splitted.head
          val fileName = splitted.last
          val newLines = File(path + / + ".sgit"+ / +"objects"+ / + "Blobs" + / + blobsha).lines
          val res = newLines.map("+"+_).toList
          printDiff(fileName,res)
        })
      }

      if (modifiedFiles.nonEmpty){
        modifiedFiles.map(line=> {
          val splitted = line.split("\t")
          val childBlobsha= splitted.head
          val fileName = splitted.last
          val newContent= File(path + / + ".sgit"+ / +"objects"+ / + "Blobs" + / + childBlobsha).lines.toList
          val parentBlobsha= parentContent.find(l=> l.split("\t").last == fileName).get.split("\t").head
          val oldContent= File(path + / + ".sgit"+ / +"objects"+ / + "Blobs" + / + parentBlobsha).lines.toList
          printDiff(fileName, getDiff(oldContent,newContent))
        })
      }
      val parentCommitRecc = getParentCommit(path, parentCommit)
      if (parentCommit!= " ") logOptionPRecc(parentCommit, parentCommitRecc)
    }
  }
  /*--------------- branch, tag command functions----------------*/

  /**
   *
   * @param refName: name of the commit or tag
   * @param branch: boolean which indicates if we want to create a branch
   * @param tag: boolean which indicates if we want to create a tag
   * @return Success or Failed message for branch or tag process
   */
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

  /**
   *
   * @return All the tags or branches existing
   */
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

  /*--------------- diff command functions----------------*/

  def diff(): Unit = {


    //1. Récupère les fichiers du WD
    val list_wdFiles = getAllFiles(path)
    val index= stage.pathAsString
    //2. On récupère les fichiers modifiés

    val modifiedOnes= getUntrackedOrModified(list_wdFiles, index).last;
    if (modifiedOnes.nonEmpty) {
      modifiedOnes.map(wdFile => {
        val newContent = wdFile.lines.toList
        val sameFileInStage= File(index).lines.toList.filter(line => line.contains(wdFile.pathAsString)).head;
        println(sameFileInStage)

        val BlobHash = sameFileInStage.split("\t").head
        val BlobName = File(path).relativize(File(sameFileInStage.split("\t").last)).toString

        val BlobPath = path + / + ".sgit" + / + "objects"+ / + "Blobs"+ / + BlobHash
        val oldContent= File(BlobPath).lines.toList

        printDiff(BlobName,getDiff(oldContent,newContent))
      })
    }
  }


  def getDiff(oldFile: List[String], newFile: List[String]): List[String] = {
    if (oldFile.isEmpty) newFile.map("+ " + _)
    else {
      if (newFile.isEmpty) oldFile.map("- " + _)
      else
        buildDiff(LCSMatrix(oldFile, newFile), oldFile, newFile)
    }
  }

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


}

object Repository{

  /*---------------init command functions----------------*/

  /**
   *
   * @param path: the path where we want to create our repository
   * @return Initialize the sgit repository if it's not already done
   */
  def initRepository(path:String): String = {
    if (!isSgitRepository(true, path)) {
      createRepository()
      "Initialized empty sGit repository in "+ path + / +".sgit" + / +""
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


  /**
   *
   * @param init:  if we are calling the function by the init process
   * @param pathToTest: path we want to check
   * @return true if is a sgit repository, false otherwise
   */
  def isSgitRepository(init: Boolean, pathToTest: String): Boolean = {
    init match {
      case true => File(s"$pathToTest${/}.sgit").exists
      case _ => getRepositoryPath(pathToTest).isDefined
    }
  }


  /**
   *
   * @param pathToTest: path from where we are calling this function
   * @return the sgit repository path by looking reccursively in the parent's folder
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


