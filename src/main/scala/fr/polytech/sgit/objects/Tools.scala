package fr.polytech.sgit.objects


import better.files._
import java.io.{File => F}

import better.files
import javax.swing.tree.TreePath

import scala.annotation.tailrec
object Tools {

  val / = F.separator

  /* --------- Tools for files ------------- */
  //Finished
  def createHash(content: String): String = {
    val md = java.security.MessageDigest.getInstance("SHA-1")
    md.digest(content.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }

  //Finished
  def createFileOrDir(name: String, isdir: Boolean, parents: Boolean): Unit = name.toFile.createIfNotExists(isdir, parents)


  //Finished
  def checklines(lines: List[String], file: wdFile): String = {

    val containsSha = lines.head.contains(file.sha)
    val containsName = lines.head.contains(file.path)
    print("sha: ")
    println(containsSha)
    print("name: ")
    println(containsName)
    //I have checked every index's lines
    val end = (lines.tail == Nil)

    (containsName, containsSha) match {
      case (true, true) => "Staged"
      case (false, _) => if (end) "Untracked" else checklines(lines.tail, file)
      case _ => "Modified"
    }
  }

  def getAllFiles(directoryPath: String): List[File] = {
    val dirFile = File(directoryPath)
    if (dirFile.exists && dirFile.isDirectory) {
      File(directoryPath).listRecursively
        .filterNot(f => f.canonicalPath.contains("/.sgit") || f.isDirectory).toList
    }
    else List()
  }

  //Finished
  def getUntrackedOrModified(listFiles:List[File], index:File): List[List[File]] = {

    def loop(filesToTest: List[File], untrackedFiles: List[File], modifiedFiles: List[File]): List[List[File]] = {

      if (filesToTest == Nil) {
        List(untrackedFiles, modifiedFiles)
      }
      else {
        val filePath = filesToTest.head
        val sha = createHash(filePath.contentAsString)
        val stateFile = getState(wdFile(sha, filePath.canonicalPath), index)
        if (stateFile == "Untracked") {
          val listRecc = filePath :: untrackedFiles
          loop(filesToTest.tail, listRecc, modifiedFiles)
        }
        else if (stateFile == "Untracked") {
          val listRecc = filePath :: modifiedFiles
          loop(filesToTest.tail, untrackedFiles, listRecc)
        } else loop(filesToTest.tail,untrackedFiles,modifiedFiles)
      }
    }

    loop(listFiles, List.empty, List.empty)
  }



  //Finished
  def getDeletedUnstaged(list_wdFiles: List[File],index: File): List[String]= index.lines.filterNot(l=> list_wdFiles.contains(l)).toList

  def getDeletedUnCommitted(list_wdFiles: List[File] ,path:String):List[String]= ???

  def geCommittedChanges(list_wdFiles: List[File] ,path:String):List[String]= ???

  def getUncommitedNewFiles(list_wdFiles: List[File] ,path:String):List[String]= ???

  /* --------- Tools for index ------------- */

  //Pas la peine de faire un foreach ou un map car tu vas parcourir tout le fichier pour rien :
  // peut etre tu t'arretes à la première ligne du fichier donc Tail reccursion
  /* Check if the file is already staged, if it's not : add it to stage else nothing */

  // Finished (Ajouté Au cas ou, meme si pas besoin pour l'instant)
  def isStaged(file: wdFile,index: File): Boolean = getState(file,index) == "Staged"
  def isModified(file: wdFile, index: File):Boolean = getState(file,index)== "Modified"
  def isUntracked(file: wdFile, index: File):Boolean = getState(file,index)== "Untracked"

  // Finished
  def getState(wdfile: wdFile, index: File): String= { val indexlines = index.lines.toList
    checklines(indexlines,wdfile)
  }

  /* --------- Tools for commit ------------- */

// Finished
  def getCurrentHead(path:String):String= {
    val head_file = File(path + / +".sgit"+ / +"HEAD").lines.head
    val head_name = head_file.split(/).last
    head_name
  }

  def updateIndexPaths(indexPaths: List[String], deep: Int): List[String] = {
    indexPaths.map(sp => if (sp.split(/).size == deep) {
      sp.split(/).dropRight(1).mkString("/")
    }
    else sp
    )
  }

  //Finished
  def getLastCommit(path: String, branch:String): String={
    val branch_file= File(path + / +".sgit"+ / +"refs"+ / +"headers"+ / +branch)
    println("branch ======" +branch_file.contentAsString)
    branch_file.contentAsString
  }

  def getTreeContent(treeSha:String, mapContent: Map[String,List[String]], path: String):Map[String,List[String]] = {
    println(File(path + / + ".sgit" + / + "objects"+ / + "Trees" + / + treeSha).exists)
    val mainTree_content = File(path + / + ".sgit" + / + "objects"+ / + "Trees" + / + treeSha).lines.toList
    val listBlobs = mainTree_content.filter(line => line.contains("blob"))
    println("listB"+ listBlobs)
    val mapTemp = mapContent + (treeSha -> listBlobs)
    val listTrees = mainTree_content.filter(line => line.contains("tree"))
    println("listT"+ listTrees)
    if (listTrees.nonEmpty) {
      val mapToReturn = listTrees.map(tree => {
        val mapSum = getTreeContent(tree.split("\t").tail.head, mapTemp, path)
        mapSum
      })
      mapToReturn.flatten.toMap
    }
    else mapTemp
  }
//  List[List[File]
  def getUncommittedChanges(index: File, path: String):List[List[String]]={

    val lastCommit = getLastCommit(path,getCurrentHead(path))
    println("llll     :: "+lastCommit.mkString(""))
    val commitTree = File(path+ / + ".sgit" + / + "objects" + / + "Commits" + / + lastCommit).contentAsString
    println("ggggg"+commitTree)
    val mapCommitTree = getTreeContent(commitTree,Map.empty,path)
    val blobs = mapCommitTree.values.flatten
    val blobs_map = blobs.map(blobLine => {
      val cm =blobLine.split("\t").tail
      cm.head -> cm.last
    }).toMap

    val parentpath = File(path).parent.canonicalPath
    val indexContent= index.lines.toList.map(line => line.split(parentpath).last)

    /* First case :  new file(s) wich had been added to index but never commited before
    --Check if file's name present in index but not in commit content*/
    val newstaged_uncommitted= indexContent.filterNot(line => blobs_map.values.toList.contains(line))


    /* Second case : These are the files that have been deleted but not commited yet
    --Check if file's name present in the last commit but not in the index  */
    val deleted_uncommittedOnes= blobs_map.filterNot(line => indexContent.contains(line._2)).values.toList


    /* Third case : These are the files that have been deleted but not commited yet
    --Check if file's name present in the last commit but not in the index */
    val modified_committedOnes = blobs_map.filter(line => indexContent.contains(line._2) && !indexContent.contains(line._1)).values.toList

    List(newstaged_uncommitted,modified_committedOnes,deleted_uncommittedOnes)
  }





}
