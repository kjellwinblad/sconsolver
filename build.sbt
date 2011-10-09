name := "SconSolver"

organization := "me.winsh"

version := "2.8.1.final"

scalacOptions ++= Seq("-deprecation")

seq(com.typesafe.sbtscalariform.ScalariformPlugin.defaultSettings: _*)

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "junit" % "junit" % "4.8" % "test->default"

libraryDependencies += "com.novocode" % "junit-interface" % "0.7" % "test->default"
