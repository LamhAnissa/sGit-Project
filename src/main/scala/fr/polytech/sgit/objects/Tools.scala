package fr.polytech.sgit.objects


import better.files._
import java.io.{File => F}

import better.files
import javax.swing.tree.TreePath

import scala.annotation.tailrec
object Tools {

  val / = F.separator

  /* --------- Tools for files ------------- */

  /**
   *
   * @param content: file's content we want to hash
   * @return the checksum by sha1
   */
  def createHash(content: String): String = {
    val md = java.security.MessageDigest.getInstance("SHA-1")
    md.digest(content.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }

  /**
   *
   * @param name: name of the file or directory
   * @param isdir: true if is a directory, false otherwise
   * @param parents: true if we want to create all the path's structure, false otherwise
   */
  def createFileOrDir(name: String, isdir: Boolean, parents: Boolean): Unit = name.toFile.createIfNotExists(isdir, parents)


  /**
   *
   * @param lines: index lines content
   * @param file: the file we want to check
   * @return
   */
  def checklines(lines: List[String], file: wdFile): String = {

    val containsSha = lines.head.contains(file.sha)
    val containsName = lines.head.contains(file.path)
    //I have checked every index's lines
    val end = (lines.tail == Nil)

    (containsName, containsSha) match {
      case (true, true) => "Staged"
      case (false, _) => if (end) "Untracked" else checklines(lines.tail, file)
      case _ => "Modified"
    }
  }

  /**
   *
   * @param directoryPath: directory on wich we want to retrieve the content reccursivly
   * @return
   */
  def getAllFiles(directoryPath: String): List[File] = {
    val dirFile = File(directoryPath)
    if (dirFile.exists && dirFile.isDirectory) {
      dirFile.listRecursively.toList.filterNot(f => f.canonicalPath.contains("/.sgit") || f.isDirectory).toList
    }
    else List()
  }

  //Retrieve  the whole content of the refs folder
  def getAllRefs(repoPath: String): List[File] = {
    val dirFile=  File(repoPath + / + ".sgit" + / + "refs")
    if (dirFile.exists && dirFile.isDirectory) {
     dirFile.listRecursively.filterNot(f =>f.isDirectory).toList
    }
    else List()
  }

  // Checkout command
  def cleanDirectory(directoryPath: String): Unit ={
    val dirFile = File(directoryPath)
    if (dirFile.exists && dirFile.isDirectory) {
      dirFile.listRecursively.toList.map(file=> file.delete())
    }
  }

  /**
   *
   * @param listFiles: files we want to check
   * @param index: index path
   * @return list containing a list of untracked files and another one with tracked but modified files
   */
  def getUntrackedOrModified(listFiles:List[File], index:String): List[List[File]] = {

    def loop(filesToTest: List[File], untrackedFiles: List[File], modifiedFiles: List[File]): List[List[File]] = {
      if (filesToTest == Nil) {
        List(untrackedFiles, modifiedFiles)
      }
      else {
        val filePath = filesToTest.head
        val sha = createHash(filePath.contentAsString)
        val stateFile = getState(wdFile(sha, filePath.canonicalPath), File(index))
        if (stateFile == "Untracked") {
          val listRecc = filePath :: untrackedFiles
          loop(filesToTest.tail, listRecc, modifiedFiles)
        }
        else if (stateFile == "Modified") {
          val listRecc = filePath :: modifiedFiles
          loop(filesToTest.tail, untrackedFiles, listRecc)
        } else loop(filesToTest.tail,untrackedFiles,modifiedFiles)
      }
    }
    if(!File(index).exists) List(getAllFiles(index.split("/.sgit").head),List.empty)
    else loop(listFiles, List.empty, List.empty)
  }


  /**
   *
   * @param list_wdFiles: list files we want to chek
   * @param index: index file
   * @return List of files which are tracked and have been deleted
   */
  def getDeletedUnstaged(list_wdFiles: List[File],index: File): List[String]= {
    val lines= index.lines.map(l=>l.split("\t").last)
    lines.filterNot(l=> {
      list_wdFiles.contains(File(l))}).toList}


  /* --------- Tools for index ------------- */


  /* Check if the file is already staged, if it's not : add it to stage else nothing */

  /**
   *
   * @param wdfile file we want to check the state
   * @param index index file
   * @return state between three possibilities : Untracked, Modified and Staged
   */
  def getState(wdfile: wdFile, index: File): String= { val indexlines = index.lines.toList
    checklines(indexlines,wdfile)
  }

  /* --------- Tools for commit ------------- */

  /**
   *
   * @param path is the repository path
   * @return branch contained in HEAD file, the current branch
   */
  def getCurrentHead(path:String):String= {
    val head_file = File(path + / +".sgit"+ / +"HEAD").lines.head
    val head_name = head_file.split(/).last
    head_name

  }

  /**
   *
   * @param indexPaths paths we want to update
   * @param deep the lenght of the paths we want
   * @return list updated of index content
   */
  def updateIndexPaths(indexPaths: List[String], deep: Int): List[String] = {
    indexPaths.map(sp => if (sp.split(/).size == deep) {
      val s= sp.split(/).dropRight(1).mkString("/")
      s
    }
    else sp
    )
  }

  /**
   *
   * @param path: repository path
   * @param branch name of the branch
   * @return sha of the last commit
   */
  def getLastCommit(path: String, branch:String): String={
    val branch_file= File(path + / +".sgit"+ / +"refs"+ / +"headers"+ / +branch)
    if (branch_file.exists) branch_file.contentAsString
    else ""
  }

  def getParentCommit(path: String, sha:String): String={
    val commit_file= File(path + / +".sgit"+ / +"objects"+ / +"Commits"+ / +sha)
    val content= commit_file.lines.toList
    val parentLine = content.tail.head
    val parent = parentLine.split("\t").last
    parent
  }

  def getContentFromCommit(path: String, sha:String): List[String]={
    val commit_file= File(path + / +".sgit"+ / +"objects"+ / +"Commits"+ / +sha)
    val content= commit_file.lines.toList

    val treeLine = content.head
    val tree = treeLine.split("\t").tail.head
    val CommitBlobsLine= getTreeContent(tree,Map.empty,path).values.toList.flatten
    val CommitBlobs = CommitBlobsLine.map(line => {
      line.split("blob\t").last
    })
    CommitBlobs
  }

  def printDiff(name: String,result : List[String]): Unit ={
    val finalDiff = result.map( line => {
      if (line.startsWith("+")) Console.GREEN + line
      else if (line.startsWith("-")) Console.RED + line
      else Console.WHITE + line
    }).mkString("\n") + "\n" + Console.WHITE
    println(Console.BLUE + "diff  --sgit  a/"+ name + "  b/"+ name +"\n\n"+ finalDiff + "\n")
  }

  def printCommitLines(path:String, sha:String): Unit ={
    val commit_file= File(path + / +".sgit"+ / +"objects"+ / +"Commits"+ / +sha).lines.toList
    val message = commit_file(2).split("\t").last
    val date = commit_file(3).split("\t").last
    println(Console.YELLOW + "commit " + sha + "  (" + Console.CYAN + "HEAD -> " + Console.GREEN + getCurrentHead(path) + Console.YELLOW + ")" +
      Console.WHITE + "\nDate: " + date + "\n\n\t" + message + "\n")

  }


  /**
   *
   * @param treeSha: sha of the current tree
   * @param mapContent: map with the whole commit tree structure
   * @param path: repository path
   * @return
   */
  def getTreeContent(treeSha:String, mapContent: Map[String,List[String]], path: String):Map[String,List[String]] = {

    val mainTree_content = File(".sgit" + / + "objects"+ / + "Trees" + / + treeSha).lines.toList

    val listBlobs = mainTree_content.filter(line => line.contains("blob"))

    val mapTemp = mapContent + (treeSha -> listBlobs)
    val listTrees = mainTree_content.filter(line => line.contains("tree"))

    if (listTrees.nonEmpty) {
      val mapToReturn = listTrees.map(tree => {
        val mapSum = getTreeContent(tree.split("\t").tail.head, mapTemp, path)
        mapSum
      })
      mapToReturn.flatten.toMap
    }
    else mapTemp
  }

  /**
   *
   * @param mapContent map with the whole commit tree structure
   * @param path repository path
   */
  def createTreeContent(mapContent: Map[String,List[String]], path: String): Unit = {

    val values = mapContent.values.toList

    val blobs = values.map(m=> m.filter((c=> c.contains("blob")))).flatten

    if(blobs.nonEmpty)createBlobObjects(blobs,path)

    val trees = values.map(m=> m.filter((c=> c.contains("tree")))).flatten
    if(trees.nonEmpty) createTrees(trees, mapContent,path)
  }

  /**
   *
   *
   * @param trees: list of trees to create
   * @param mapDir: map with the trees and their contents
   * @param path repository path
   */
  def createTrees(trees:List[String],mapDir: Map[String,List[String]],path: String): Unit ={

    if (trees == Nil){}
    else{
      val tree= trees.head
      val tree_path= tree.split("\t").last
      val tree_name= tree_path.split(/).last
      val children= mapDir.get(tree_name).get
      val treeContent = children.mkString("\n")
      val tree_sha = createHash(treeContent)
      val tree_filepath = path + / + ".sgit" + / + "objects" + / + "Trees" +  / +tree_sha

      // create tree file hash if not already created
      if (!File(tree_filepath).exists) { val tree_file = File(tree_filepath).createIfNotExists(false)
        tree_file.appendLine(treeContent)}
      createTrees(trees.tail,mapDir,path)
    }
  }


  /**
   *
   * @param files list of blobs to create
   * @param path repository path
   */
  def createBlobObjects(files:List[String],path:String): Unit = {

    if(files == Nil){}
    else{

      val file_args= files.head.split("\t").toList
      val blob_sha=  file_args.tail.head
      val file_path= file_args.last.split(/).drop(1).mkString(/)

      val blob_path= path + / + ".sgit" + / +"objects" + / + "Blobs" + / + blob_sha
      if (!File(blob_path).exists) {
       File(file_path).copyTo(File(blob_path))
      }
      createBlobObjects(files.tail,path)}

  }

  /**
   *
   * @param index: index file
   * @param path: repository path
   * @return list with : -list og files staged but not ever commited, thenewstaged_uncommitted,modified_committedOnes,deleted_uncommittedOnes)
   *         list of files staged already commited but then modified
   *         list of files staged but then deleted
   */
  def getUncommittedChanges(index: File, path: String):List[List[String]]={

    val lastCommit = getLastCommit(path,getCurrentHead(path))
    if (lastCommit!=""){
    val commitContent = File(path+ / + ".sgit" + / + "objects" + / + "Commits" + / + lastCommit).contentAsString

    val commitTreeLine= commitContent.split("\n").head

    val commitTree = commitTreeLine.split("\t").tail.head

    val mapCommitTree = getTreeContent(commitTree,Map.empty,path)


    val blobs = mapCommitTree.values.flatten


    val blobs_map = blobs.map(blobLine => {
      val cm =blobLine.split("\t").tail
       cm.last -> cm.head
    }).toMap

      val parentpath = File(path).parent.pathAsString + /


    val indexContent= index.lines.toList.map(line => {
      val name = line.split(parentpath).last
      val sha = line.split("\t").head
      name -> sha
    }).toMap


    /* First case :  new file(s) which had been added to index but never commited before
    --Check if file's name present in index but not in commit content*/

    val newstaged_uncommitted= indexContent.filterNot(line => {
      blobs_map.keys.toList.contains(line._1)
    }).keys.toList



    /* Second case : These are the files that have been deleted but not commited yet
    --Check if file's name present in the last commit but not in the index  */
    val deleted_uncommittedOnes= blobs_map.filterNot(line => indexContent.contains(line._1)).keys.toList


    /* Third case : These are the files that have been modified but not commited yet
    --Check if file's sha present in the last commit but not in the index */

   val modified_committedOnes1 =blobs_map.filter(line => indexContent.keys.toList.contains(line._1) && !indexContent.values.toList.contains(line._2))
     val modified_committedOnes = modified_committedOnes1.keys.toList


    //  val modified_committedOnes = List.empty
    List(newstaged_uncommitted,modified_committedOnes,deleted_uncommittedOnes)}

   else {val newFiles = index.lines.toList.map(l=> l.split("\t").last)
      List(newFiles,List.empty,List.empty)}
  }





}
