package fr.polytech.sgit.tests


import org.scalatest._
import better.files._
import java.nio.file.{Files, Paths}

import fr.polytech.sgit.objects.Repository

class initTest extends FunSpec {


  describe("With no sgit init command used beforehand") {
    it("Should create a .sgit directory with the right structure") {
      if(Files.exists(Paths.get(".sgit"))) Tools.delete(".sgit")
      Init.sgit_init()
      assert(Files.exists(Paths.get(".sgit/index")))
      assert(Files.exists(Paths.get(".sgit/objects")))
      assert(Files.exists(Paths.get(".sgit/refs/tags")))
      assert(Files.exists(Paths.get(".sgit/refs/heads")))
      assert(Files.readString(Paths.get(".sgit/HEAD")) == "ref: refs/heads/master")
    }
  }

  describe("With sgit init command already used") {
    it("Shouldn't do anything") {
      assert(Init.sgit_init() == "Initialised empty sGit repository in " + Paths.get(".").toAbsolutePath + ".sgit")
      assert(Init.sgit_init() == "Reinitialised existing sGit repository in " + Paths.get(".").toAbsolutePath + ".sgit")
    }
  }
}