name := "io.lbert.category-scala"

version := "1.0"

scalaVersion := "2.12.0"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.typelevel" %% "cats" % "0.8.1",
  "com.chuusai" %% "shapeless" % "2.3.3"
)

