package fr.polytech.sgit.objects

import better.files.File

case class Repository(val path: String) {

/* Initialize the sgit repository if it's not already done
 */
  val sep= f.separator
  val stage = File(s"$path$sep.sgit${sep}index")

  def initRepository(): String = {
    if (!isSgitRepository(true, path)) {
      createRepository()
      "Initialised empty sGit repository in " + path + sep +".sgit"
    }
    else {
     "Unable to create a repository here:" +path+sep+"sgit already existing"
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


  def add(files: Seq[String]):String ={
    val fails = Nil
    val validNames = Nil

    files.map( f => {
      if (File(f).exists) f :: validNames
      else f :: fails
    })
    val tostring= fails.mkString(",")

     if (toString==""){
       addFilesToStage(validNames)
       "Success files have been added"
     }
    else " The following files:" + tostring + " could not be added cause they did not match any files"

  }


  def addFilesToStage(filesToBeStaged: List[File]): Unit = {

    val indexContent = stage.lines.toList

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
      CheckIn_Index(filesToBeStaged)
    }
  }

  def CheckIn_Index(files: List[File]): Unit = {

    val indexContent = stage.lines.toList
    val fileIt = files.head
    // File information
    val sha = createHash(fileIt.contentAsString)


    val state= getState(indexContent,fileIt,sha)

    if (state=="Untracked") stage.appendLine(s"$sha\t${fileIt.canonicalPath}")
    else if (state=="Modified")
    if(!(files.tail == Nil)){
      CheckIn_Index(files.tail)
    }
  }

  //Pas la peine de faire un foreach ou un map car tu vas parcourir tout le fichier pour rien :
  // peut etre tu t'arretes à la première ligne du fichier
  /* Check if the file is already staged, if it's not : add it to stage else nothing */

  def isStaged(lines: List[String], file: File): Boolean = getState(lines,file)=="staged"
  def isModified(lines: List[String], file: File):Boolean = getState(lines,file)=="modified"
  def isUntracked(lines: List[String], file: File):Boolean = getState(lines,file)=="modified"

  def getState(lines: List[String], file: File,sha: String): String= {


    val containsSha = lines.head.contains(file)
    val containsName = lines.head.contains(sha)

    //I have checked every index's lines
    val end = (lines.tail == Nil)

    (containsName,containsSha) match{
      case(true,true) => "staged"
      case(false,false) => if (end) {
        stage.appendLine(s"$sha\t${file.canonicalPath}")
        "untracked"
      } else getState(lines.tail, file,sha)
      case _ => "modified"

    }

  }

  def createHash(content: String): String = {
    val md = java.security.MessageDigest.getInstance("SHA-1")
    md.digest(content.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }

}
