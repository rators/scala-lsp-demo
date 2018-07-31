

resolvers ++= Seq("bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven",
                  "dhpcs at bintray" at "https://dl.bintray.com/dhpcs/maven")

name := "scala-lsp-demo"

version := "0.1"

scalaVersion := "2.12.6"

val ParsebackVersion = "0.3"

libraryDependencies ++= Seq("com.lihaoyi" %% "ammonite-ops" % "1.1.2",
                            "org.antlr" % "antlr4" % "4.7.1",
                            "org.scalactic" %% "scalactic" % "3.0.5",
                            "com.github.dragos" % "languageserver_2.12" % "0.2.3",
                            "com.codecommit" %% "parseback-core" % ParsebackVersion,
                            "com.codecommit" %% "parseback-cats" % ParsebackVersion,
                            "org.scalatest" %% "scalatest" % "3.0.5" % Test
)