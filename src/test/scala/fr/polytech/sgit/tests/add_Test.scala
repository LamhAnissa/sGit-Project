package fr.polytech.sgit.tests
import java.io.{File=>f}

import fr.polytech.sgit.objects.Repository
import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import better.files._
class add_Test extends FlatSpec with BeforeAndAfterEach{

  val / = f.separator
  val virginRepo = Repository(System.getProperty("user.dir"))
  val indexPath: String = virginRepo.path + / +".sgit/index"
  val filesforTests: List[String] = List("helloWorld.txt", "lapin.txt", "lapin_copie.txt")

    //Create all the examples file test needed.
    override def beforeEach(): Unit = {
      virginRepo.initRepository()
      "helloWorld.txt".toFile.createIfNotExists(false,false).appendLine(" Hello World !!")
      "lapin.txt".toFile.createIfNotExists(false,false).appendLine(" Lapin !!")
      "lapin_copie.txt".toFile.createIfNotExists(false,false).appendLine(" Lapin !!")
    }

    //Delete examples test files and the .sgit directory after each test
    override def afterEach(): Unit = {
      File(indexPath).delete()
      filesforTests.map(f => File(f).delete())

    }

    " The sgit add command" should "create index file in .sgit if it's the first time we call sgit add <filename> " in {
      assert(0 == 0)
      assert(!(File(indexPath).exists))
      virginRepo.add(filesforTests)
      assert(File(indexPath).exists())
    }

    it should "insert new line by each parameters which are not present in the index file, each line containing <sha>\t<filepath> of the parameter" in {
       //Choose the 2 last files in the list of example : lapin.txt, lapin_copie.txt
       val file1_path = File(filesforTests.tail.head).canonicalPath
       val file1_sha = virginRepo.createHash(file1_path)
       val file2_path = File(filesforTests.last).canonicalPath
       val file2_sha = virginRepo.createHash(file2_path)

       virginRepo.add(Seq(file1_path,file2_path))

       assert((File(indexPath).lines.size==2))
      assert((File(indexPath).lines.toList.filter(l => l.contains(file1_sha+ "\t" +file1_path))).isEmpty)
      assert((File(indexPath).lines.toList.filter(l => l.contains(file2_sha+ "\t" +file2_path))).isEmpty)
    }


}
