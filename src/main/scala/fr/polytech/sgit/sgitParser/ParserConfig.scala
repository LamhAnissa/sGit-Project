package fr.polytech.sgit.sgitParser

import java.io.File

case class ParserConfig(command: String = "",
                        option: String = "",
                        files: Seq[String] = Seq(),
                        element: String = ""
                       )
