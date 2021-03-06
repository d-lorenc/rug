package com.atomist.rug.kind.yml

import com.atomist.param.SimpleParameterValues
import com.atomist.project.edit.{NoModificationNeeded, SuccessfulModification}
import com.atomist.rug.DefaultRugPipeline
import com.atomist.rug.InterpreterRugPipeline.DefaultRugArchive
import com.atomist.rug.TestUtils.attemptModification
import com.atomist.source._
import org.scalatest.{FlatSpec, Matchers}
import org.yaml.snakeyaml.Yaml

abstract class AbstractYmlUsageTest extends FlatSpec with Matchers {

  private val parser = new Yaml()

  protected def runProgAndCheck(prog: String, as: ArtifactSource, mods: Int): ArtifactSource = {
    val progArtifact: ArtifactSource = new SimpleFileBasedArtifactSource(DefaultRugArchive,
      StringFileArtifact(new DefaultRugPipeline().defaultFilenameFor(prog), prog)
    )

    val modAttempt = attemptModification(progArtifact, as, EmptyArtifactSource(""), SimpleParameterValues.Empty)

    modAttempt match {
      case sm: SuccessfulModification =>
        assert(sm.result.cachedDeltas.size === mods)
        sm.result.cachedDeltas.foreach {
          case fud: FileUpdateDelta =>
            // TODO how do we validate YML? SnakeYAML seems to let everything through
          case x => fail(s"Unexpected change: $x")
        }
        sm.result
      case _: NoModificationNeeded if mods > 0 =>
        fail(s"No modification made when $mods expected: $prog; \n${ArtifactSourceUtils.prettyListFiles(as)}")
      case _: NoModificationNeeded if mods == 0 =>
        as
    }
  }

}
