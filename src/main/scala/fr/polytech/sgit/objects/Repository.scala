package fr.polytech.sgit.objects
import java.io
import java.io.{File=>f}


import better.files._

case class Repository(val path: String){

/* Initialize the sgit repository if it's not already done
 */
  val sep= f.separator
  val stage = File(s"$path$sep.sgit${sep}index")

  def initRepository(): Unit = {
    println("Je suis dans le init")
    if (!isSgitRepository(true, path)) {
      createRepository()
    }
    else {
      println(s"Failed, unable to create repository : $path${sep}sgit already exists")
    }
  }


  /* Create the whole sgit folder tree structure */
  def createRepository() : Unit = {

    createFileOrDir(".sgit/Objects/Commits", true, true)
    createFileOrDir(".sgit/Objects/Trees", true, false)
    createFileOrDir(".sgit/Objects/Blobs", true, false)

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


  def add(files: Array[String]):Unit ={
    val fails = Nil
    val validNames = Nil

    files.map( f => {
      val fileToBeStaged = File(f)
      if (fileToBeStaged.exists) fileToBeStaged :: validNames
      else fileToBeStaged :: fails
    })

    if (validNames.nonEmpty){
      addFilesToStage(validNames)
    }
    val tostring= fails.mkString(",")
    if(!(tostring==null)){
      println(s" The following files $tostring did not match any files")
    }

  }


  def addFilesToStage(filesToBeStaged: List[File]): Unit = {

    //Retrieve index content

    val indexContent = stage.lines

    //val file = filesToBeStaged.head

    if (!stage.exists) {
      stage.createFile()
      filesToBeStaged.foreach(f => {
        val fileContent = f.contentAsString
        val sha = createHash(fileContent)
        stage.appendLine(s"$sha\t${f.canonicalPath}")
      })
    }
    else{
      AreInIndex(filesToBeStaged,stage.lines.toList)
    }
  }

  def createHash(content: String): String = {
    val md = java.security.MessageDigest.getInstance("SHA-1")
    md.digest(content.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }

  def AreInIndex(files: List[File],indexContent: List[String]): Unit = {

    val fileIt = files.head
    isStaged(indexContent,fileIt)
    if(!(files.tail == Nil)){
      AreInIndex(files.tail, indexContent)
    }
  }

  //Pas la peine de faire un foreach ou un map car tu vas parcourir tout le fichier pour rien :
  // peut etre tu t'arretes à la première ligne du fichier
  /* Check if the file is already staged, if it's not : add it to stage else nothing */
  def isStaged(lines: List[String], file: File): Boolean = checkFileInIndex(lines,file)

  def checkFileInIndex(lines: List[String], file: File): Boolean = {

    val sha = createHash(file.contentAsString)
    val line = lines.head
    val contains = line.contains(sha)
    val end = (lines.tail == Nil)
    (contains, end) match {
      case (false, false) => checkFileInIndex(lines.tail, file)
      case (false, true) => {
        stage.appendLine(s"$sha\t${file.canonicalPath}")
        false
      }
      case _ => true
    }
  }

}

