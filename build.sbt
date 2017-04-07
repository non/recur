name := "recur"

scalaVersion := "2.12.1"

libraryDependencies ++=
  "org.typelevel" %% "cats" % "0.9.0" ::
  Nil

scalacOptions ++=
  "-feature" ::
  "-language:higherKinds" ::
  "-Ypartial-unification" ::
  Nil
