val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "test-git",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-deprecation",
      "-explain"
    ),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
  )

// lazy val baseProject = RootProject(
//   uri("git@github.com:hmc-cs111-spring2023/cs111-hw5-bwiedermann-hw5.git")
// )
