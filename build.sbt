ThisBuild / scalaVersion := "2.12.7"
ThisBuild / organization := "com.battleship"

val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
//TODO : rajouter tailRec
lazy val battleship = (project in file("."))
  .settings(
    name := "battleship",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += opencsv,
  )
val opencsv = "au.com.bytecode" % "opencsv" % "2.4"

