package fr.polytech.sgit.objects

trait GitObject{
  val hash : String = ""
  val content : String // should be Byte
  def getHash(): String
}
