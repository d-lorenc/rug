package com.atomist.rug.runtime.execution

import com.atomist.param.SimpleParameterValues
import com.atomist.project.edit.ProjectEditor
import com.atomist.project.generate.ProjectGenerator
import com.atomist.rug.runtime.AddressableRug
import com.atomist.rug.spi.Handlers.Instruction._
import com.atomist.rug.spi.Handlers.{Instruction, Response}
import com.atomist.source.ArtifactSource

/**
  * Run instructions synchronously in this JVM
  *
  * TODO - ensure we blow up if there are rugs or instructions with duplicate names
  * and don't have different GA's
  */
class LocalInstructionRunner(rugs: Seq[AddressableRug],
                            projectFinder: ProjectFinder,
                            projectPersister: ProjectPersister)
  extends InstructionExecutor{
  override def execute(instruction: Instruction, callbackInput: Object): Response = {
    instruction match {
      case editor: Edit => {
        rugs.find(p => matches(p, editor.detail)) match {
          case rug: ProjectEditor => rug.modify(projectFinder.findArtifactSource(instruction.detail.projectName.get), SimpleParameterValues(editor.detail.parameters))
          case _ => ???
        }
      }

      case editor: Generate => {
        val newproject = rugs.find(p => matches(p, editor.detail)) match {
          case rug: ProjectGenerator => rug.generate(editor.detail.projectName.get, SimpleParameterValues(editor.detail.parameters))
          case _ => ???
        }
        projectPersister.persist(editor.detail.projectName.get,newproject)
      }
        //TODO review - just like above. Put results in Response for a potential response handler
        //TODO execute - look up a JVM object from a registry somewhere, run it, and return the result. Use JVM ServiceLocator stuff. Just like a Rug, but in pure JVM.

        //TODO command - look up a Rug just like above (CommandHandler)

        //TODO respond - also a Rug (CommandHandler)

      case _ =>
    }
    Response(null,None,None,None)
  }

  /**
    * //TODO change to find matching Rug and validate type!
    *
    * Does the rug match the instruction?
    * @param rug
    * @param detail
    * @return
    */
  private def matches(rug: AddressableRug, detail: Detail): Boolean = {
    val nameMatches = detail.name == rug.name
    if(detail.coordinates.nonEmpty){
      val coords = detail.coordinates.get
      nameMatches && rug.group == coords.group && rug.name == coords.artifact
    }else{
      nameMatches
    }
  }
}
trait ProjectFinder {
  def findArtifactSource(project_name: String) : ArtifactSource
}

trait ProjectPersister{
  def persist(projectName: String, source: ArtifactSource)
}
