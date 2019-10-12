package fr.polytech.sgit.objects

case class Blob() extends GitObject{

  override def getHash(): String = ""
  override val content: String = ""
}
