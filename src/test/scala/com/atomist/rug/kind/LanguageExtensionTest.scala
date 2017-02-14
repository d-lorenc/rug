package com.atomist.rug.kind

import _root_.java.io.File

import com.atomist.rug.kind.core.ProjectMutableView
import com.atomist.source.file.{FileSystemArtifactSource, FileSystemArtifactSourceIdentifier}
import com.atomist.tree.UpdatableTreeNode
import com.atomist.tree.pathexpression.{PathExpressionEngine, PathExpressionParser}

/**
  * Utilities for testing a Rug language expression
  */
trait LanguageExtensionTest {

  def projectFromDirectory(path: String): ProjectMutableView =
    new ProjectMutableView(FileSystemArtifactSource(FileSystemArtifactSourceIdentifier(new File(path))))

  val pxe = new PathExpressionEngine()
  val parser = PathExpressionParser

  def evaluatePathExpression(project: ProjectMutableView, expression: String): Seq[UpdatableTreeNode] = {
    val parsed = parser.parseString(expression)
    pxe.evaluate(project, parsed) match {
      case Left(error) => throw new RuntimeException(s"Could not parse path expression $expression: $error")
      case Right(matches) => matches.map(_.asInstanceOf[UpdatableTreeNode])
    }
  }

}

object LanguageExtensionTest extends LanguageExtensionTest
