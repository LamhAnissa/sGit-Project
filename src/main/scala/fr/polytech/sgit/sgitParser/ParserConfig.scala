package fr.polytech.sgit.sgitParser

case class ParserConfig(command: String = "",
                        option: String = "",
                        files: Seq[String] = Seq(),
                        element: String = ""
                       )
