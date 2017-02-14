package com.atomist.rug.kind

import _root_.java.io.File

import com.atomist.rug.kind.core.ProjectMutableView
import com.atomist.source.file.{FileSystemArtifactSource, FileSystemArtifactSourceIdentifier}

/**
  * Utilities for testing a Rug language expression
  */
trait LanguageExtensionTest {

  def projectFromDirectory(path: String): ProjectMutableView =
    new ProjectMutableView(FileSystemArtifactSource(FileSystemArtifactSourceIdentifier(new File(path))))

}

object LanguageExtensionTest extends LanguageExtensionTest
