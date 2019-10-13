package fr.polytech.sgit.tests

import fr.polytech.sgit.objects.Repository
import org.scalatest._
import better.files.{File=> BTFile}
import java.io.File


class init_Test extends FlatSpec with BeforeAndAfterEach {

  val / = File.separator
  val testRepository = Repository(System.getProperty("user.dir"))

    //Mock sgit init before each test
    override def beforeEach(): Unit = {
      testRepository.initRepository()
    }

    //Clean the working repository after each test
    override def afterEach(): Unit = {
      val sgit_directory= testRepository.path + / + ".sgit"
      BTFile(sgit_directory).delete()
    }


    "The sGit <init> command" should "create the .sgit repository with the right structure" in {
      assert(BTFile(".sgit").exists())
      assert(BTFile(".sgit" + / + "objects/Blobs").exists())
      assert(BTFile(".sgit" + / + "objects/Commits").exists())
      assert(BTFile(".sgit" + / + "objects/Trees").exists())
      assert(BTFile(".sgit" + / + "refs/tags").exists())
      assert(BTFile(".sgit" + / + "refs/headers").exists())
      assert(BTFile(".sgit" + / + "HEAD").exists())

    }

    it should "insert \"master\" as initial branch in HEAD's content" in {
      val content = BTFile(".sgit" + / + "HEAD").lines.mkString("")
      assert(content == "ref: refs/heads/master")
    }

    it should "not be possible to initialize a sgit repository if it has been already done" in {
      val pathBis = System.getProperty("user.dir")
      val repo = Repository(pathBis)
      assert(repo.initRepository() == ("Unable to create a repository here:" + pathBis + / + "sgit already existing"))
    }



  }