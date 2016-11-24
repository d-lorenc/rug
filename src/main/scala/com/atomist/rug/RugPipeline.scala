package com.atomist.rug

import com.atomist.project.ProjectOperation
import com.atomist.project.archive.AtomistConfig
import com.atomist.source.{StringFileArtifact, SimpleFileBasedArtifactSource, ArtifactSource, FileArtifact}

/**
  * Interface for compiling Rug files
  */
trait RugPipeline {

  def atomistConfig: AtomistConfig

  /**
    * Parse programs in the archive
    *
    * @param rugArchive      the artifact source
    * @param namespace       namespace to put us in if known
    * @param knownOperations known other operations we might reference
    * @throws BadRugException
    * @throws IllegalArgumentException
    * @return a sequence of ProjectOperations
    */
  @throws[BadRugException]
  @throws[IllegalArgumentException]
  def create(rugArchive: ArtifactSource,
             namespace: Option[String],
             knownOperations: Seq[ProjectOperation] = Nil): Seq[ProjectOperation]

  @throws[BadRugException]
  @throws[IllegalArgumentException]
  final def createFromString(input: String,
                             namespace: Option[String] = None,
                             otherOperations: Seq[ProjectOperation] = Nil): Seq[ProjectOperation] = {
    import InterpreterRugPipeline._

    val as =
      new SimpleFileBasedArtifactSource(DefaultRugArchive,
        StringFileArtifact(atomistConfig.defaultRugFilepath, input))
    create(as, namespace, otherOperations)
  }

  @throws[BadRugPackagingException]
  def validatePackaging(rugFiles: Seq[FileArtifact], f: FileArtifact, progs: Seq[RugProgram]): Unit
}

