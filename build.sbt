import sbt._
import sbt.Keys._

lazy val validating = project.in(file("."))

organization := "es.weso"

name := "validating"

version := "0.0.16"

scalaVersion := "2.11.8"

publishMavenStyle := true

libraryDependencies ++= Seq(
  compilerPlugin("org.spire-math" %% "kind-projector"   % "0.8.0")
, compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)
, compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
, "org.scalatest" %%% "scalatest" % "3.0.0-RC2" % "test"
, "org.typelevel" %% "cats" % "0.7.0"
, "org.atnos" %% "eff-cats" % "2.0.0-RC6"
)

bintrayRepository in bintray := "weso-releases"

bintrayOrganization in bintray := Some("weso")

licenses += ("MPL-2.0", url("http://opensource.org/licenses/MPL-2.0"))

resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

// EclipseKeys.useProjectId := true

// Publish site info
site.settings

site.publishSite

site.includeScaladoc()

ghpages.settings

git.remoteRepo := "git@github.com:labra/validating.git"

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/labra/validating")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/labra/validating"), "scm:git:git@github.com:labra/validating.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://labra.github.io/validating/latest/api/")),
  pomExtra := (
    <developers>
      <developer>
        <id>labra</id>
        <name>Jose Emilio Labra</name>
        <url>https://github.com/labra/</url>
      </developer>
    </developers>
  ),
  scalacOptions in (Compile,doc) ++= Seq(
      "-language:existentials"
    , "-language:higherKinds"
    , "-language:implicitConversions"
    , "-unchecked"
    , "-Xfatal-warnings"
    , "-Xlint"
    , "-Yno-adapted-args"
    , "-Ywarn-dead-code"
    , "-Ywarn-numeric-widen"
    , "-Ywarn-value-discard"
    , "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala"
    , "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath
    , "-diagrams"
  )
)
