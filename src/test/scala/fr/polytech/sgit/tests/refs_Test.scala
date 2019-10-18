package fr.polytech.sgit.tests



import fr.polytech.sgit.objects.Repository
import fr.polytech.sgit.objects.Tools
import org.scalatest._
import better.files.{File => BTFile}
import java.io.File
import scala.reflect.internal.util.FileUtils



  class refs_Test extends FlatSpec with BeforeAndAfterEach {


    val / = File.separator
    val testRepository = Repository(System.getProperty("user.dir"))


    override def beforeEach(): Unit = {

      Repository.initRepository(System.getProperty("user.dir"))
    }

    //Clean the working repository after each test
    override def afterEach(): Unit = {
      Tools.cleanDirectory(".sgit"+ / + "refs"+ / + "headers")
      Tools.cleanDirectory(".sgit"+ / + "refs"+ / + "tags")
      val master = BTFile(".sgit"+ / + "refs"+ / + "headers" + / +"master").createIfNotExists()
      val headFile = BTFile(".sgit"+ / + "HEAD")
      headFile.writeText("ref: refs"+ / +"heads"+ / + "master")
      val logFile = BTFile(".sgit"+ / + "log").delete()
    }
    //For sgit branch command
    "The sGit branch <branchName> command" should "create a file with branchName as name, last commit hash as content, in .sgit/refs/headers" in {
        val newBranch= "imthenewBranch"
        val newBranchFile = BTFile(".sgit"+ / + "refs" + / + "headers"+ / + newBranch)
        val branchTest = BTFile(".sgit"+ / + "refs" + / + "headers"+ / + "branchTest").createIfNotExists(false).appendLine("hashOfLastCommit")
        val headFile = BTFile(".sgit"+ / + "HEAD")
        headFile.writeText("ref: refs"+ / +"heads"+ / + branchTest.pathAsString.split(/).last)

         // Simulate a commit consequence instead of mocking a Commit
         val logFile = BTFile(".sgit"+ / + "log").createIfNotExists()


        //File of new branch successfully created with right path
        assert(testRepository.createTagOrCommit(newBranch,true,false) == s"$newBranch successfully created")
        assert(newBranchFile.exists)

      //File of new branch contains the last commit of the current branch
        assert(newBranchFile.contentAsString.contains("hashOfLastCommit"))
    }

    it should "not be possible to create a branch with the same name of an existing branch or tag " in {
      // Simulate a commit consequence instead of mocking a Commit
      val logFile = BTFile(".sgit"+ / + "log").createIfNotExists()
      val newBranch= "imthenewBranch"
      val existingBranch = BTFile(".sgit"+ / + "refs" + / + "headers"+ / + newBranch).createIfNotExists(false)
      assert(testRepository.createTagOrCommit(newBranch,true,false) == "This name is already associated to an existing branch or tag")
    }

    it should "not be possible to create a branch if no commits <=> no log file)" in {
      val newBranch= "imthenewBranch"
      val lapin= "lapin"
      assert(testRepository.createTagOrCommit(newBranch,true,false).contains("Not able to create branches or tags if no existing commits") )
      testRepository.createTagOrCommit(lapin,true,false)
      assert(!BTFile(".sgit"+ / + "refs" + / + "headers"+ / + lapin).exists)
    }

    // For sgit branch --av
    it should "with the --av option list all the branch and tags" in {

      // Simulate a commit consequence instead of mocking a Commit
      val logFile = BTFile(".sgit"+ / + "log").createIfNotExists()

      //Create new branches to test the function
      val listnewBranches = List("branch1", "branch2")
      val listnewTags = List("tag1", "tag2")
      listnewBranches.map(branch=> testRepository.createTagOrCommit(branch,true,false))
      listnewTags.map(tag=> testRepository.createTagOrCommit(tag,false,true))
      assert(testRepository.listAllRefs().contains("- " + listnewBranches.head + ": branch"))
      assert(testRepository.listAllRefs().contains("- " + listnewBranches.last + ": branch"))
      assert(testRepository.listAllRefs().contains("- " + listnewTags.head + ": tag"))
      assert(testRepository.listAllRefs().contains("- " + listnewTags.last + ": tag"))
    }

    //For sgit tag command
    "The sGit tag <tagName> command" should "create a file with tagName as name, last commit hash as content, in .sgit/refs/tags" in {

      val newTag= "imthenewTag"
      val newTagFile = BTFile(".sgit"+ / + "refs" + / + "tags"+ / + newTag)
      val tagTest = BTFile(".sgit"+ / + "refs" + / + "tags"+ / + "tagTest").createIfNotExists(false).appendLine("hashOfLastCommit")


      // Simulate a commit consequence instead of mocking a Commit
      val logFile = BTFile(".sgit"+ / + "log").createIfNotExists()

      //File of new tag successfully created with right path
      assert(testRepository.createTagOrCommit(newTag,false,true) == s"$newTag successfully created")
      assert(newTagFile.exists)

      //File of new branch contains the last commit of the current branch
      assert(newTagFile.contentAsString == "hashOfLastCommit")
    }

    it should "not be possible to create a tag with the same name of an existing branch or tag " in {

      // Simulate a commit consequence instead of mocking a Commit
      val logFile = BTFile(".sgit"+ / + "log").createIfNotExists()

      val newTag= "imthenewTag"
      val existingBranch = BTFile(".sgit"+ / + "refs" + / + "headers"+ / + newTag).createIfNotExists(false)
      assert(testRepository.createTagOrCommit(newTag,false,true) == "This name is already associated to an existing branch or tag")
    }

    it should "not be possible to create a tag if no commits <=> no log file)" in {
      val newTag= "imthenewTag"
      assert(testRepository.createTagOrCommit(newTag,false,true).contains("Not able to create branches or tags if no existing commits"))
      assert(!BTFile(".sgit"+ / + "refs" + / + "headers"+ / + newTag).exists)
    }

  }


