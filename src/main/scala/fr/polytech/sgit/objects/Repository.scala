package fr.polytech.sgit.objects

import better.files._
import java.io.{File => f, FileWriter}
import wdFile._

case class Repository(val path: String) {

  /*---------------init command functions----------------*/

/* Initialize the sgit repository if it's not already done
 */
  val sep= f.separator
  val stage = File(s"$path${sep}.sgit${sep}index")

  def initRepository(): String = {
    if (!isSgitRepository(true, path)) {
      createRepository()
      "Initialised empty sGit repository in " + path + sep +".sgit"
    }
    else {
     "Unable to create a repository here:" + path + sep +"sgit already existing"
    }
  }


  /* Create the whole sgit folder tree structure */
  def createRepository() : Unit = {

    createFileOrDir(".sgit/objects/Commits", true, true)
    createFileOrDir(".sgit/objects/Trees", true, false)
    createFileOrDir(".sgit/objects/Blobs", true, false)

    createFileOrDir(".sgit/refs/tags", true, true)
    createFileOrDir(".sgit/refs/headers", true, false)
    ".sgit/HEAD".toFile.createIfNotExists(false,false).appendLine("ref: refs/heads/master\n")
  }

  /* Create File or Directory w/o parents */
  def createFileOrDir(name: String, isdir: Boolean ,parents: Boolean): Unit = name.toFile.createIfNotExists(isdir, parents)

  /* Check if we are in a sgit repository. Look for the .sgit in current directory, if we haven't returned,
  recurse in parent
   */
  def isSgitRepository(init: Boolean, pathToTest: String): Boolean = {

    val possible_sgitpath = new f(s"$pathToTest${sep}.sgit")
    val currentDirectory = new f(pathToTest)
    val found = possible_sgitpath.exists()

    // If init=true that means that we have just to check the current directory,
    // else we have to check the whole tree structure
    init match {
      case true => found
      case _ => {

        val parentDirectory = currentDirectory.getParent
        if(!found && !(parentDirectory == null)){
        isSgitRepository(false,parentDirectory)
        }
        else found
      }
    }
  }

  /*-----------add command functions----------------*/

  //Finished
  def add(files: Seq[String]):String ={
    var fails = List() : List[String]
    var validNames = List() : List[File]

    files.toList.map( f => {
      val filef= File(f)
      if (filef.exists) validNames = filef :: validNames
      else  fails = f :: fails
    })
    val tostring= fails.mkString(", ")
     if (fails.isEmpty){
       addFilesToStage(validNames)
       "Success files have been added"
     }
    else " The following files: " + tostring + " could not be added cause they did not match any files"

  }

  //Finished
  def addFilesToStage(filesToBeStaged: List[File]): Unit = {

    if (!stage.exists) {
      stage.createFile()
      filesToBeStaged.map(f => {
        val fileContent = f.contentAsString
        val sha = createHash(fileContent)
        stage.appendLine(s"$sha\t${f.canonicalPath}")
      })
    }
    else filesToBeStaged.map(f=> UpdateIfNeeded(f))
  }

  //Finished
  def UpdateIfNeeded(file: File): Unit = {

    val sha = createHash(file.contentAsString)
    val filepath = file.canonicalPath
    val wdfile= wdFile(sha,filepath)
    val res= getState(wdfile)

    if (!(res == "Staged")){
      if (res == "Modified"){
        val newcontent= stage.lines.toList.filterNot(l => l.contains(filepath))
        val filewriter= new FileWriter(stage.canonicalPath)
        filewriter.write(newcontent.mkString("\n")+"\n")
        filewriter.close()
      }
      stage.appendLine(s"$sha\t$filepath")
    }
  }

  //Pas la peine de faire un foreach ou un map car tu vas parcourir tout le fichier pour rien :
  // peut etre tu t'arretes à la première ligne du fichier donc Tail reccursion
  /* Check if the file is already staged, if it's not : add it to stage else nothing */

  // Finished (Ajouté Au cas ou, meme si pas besoin pour l'instant)
  def isStaged(file: wdFile): Boolean = getState(file) == "Staged"
  def isModified(file: wdFile):Boolean = getState(file)== "Modified"
  def isUntracked(file: wdFile):Boolean = getState(file)== "Untracked"

  // Finished
  def getState(wdfile: wdFile): String= { val indexlines = stage.lines.toList
    checklines(indexlines,wdfile)}

  // Finished
  def checklines(lines:List[String], file:wdFile):String ={

    val containsSha = lines.head.contains(file.sha)
    val containsName = lines.head.contains(file.path)
    print("sha: ")
    println(containsSha)
    print("name: ")
    println(containsName)
    //I have checked every index's lines
    val end = (lines.tail == Nil)

    (containsName,containsSha) match{
      case(true,true) => "Staged"
      case(false,_) => if (end) "Untracked" else checklines(lines.tail,file)
      case _ => "Modified"
    }
  }

  //Finished
  def createHash(content: String): String = {
    val md = java.security.MessageDigest.getInstance("SHA-1")
    md.digest(content.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }

}
