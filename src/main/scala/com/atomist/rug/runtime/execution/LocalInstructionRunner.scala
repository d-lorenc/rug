package com.atomist.rug.runtime.execution

import com.atomist.param.SimpleParameterValues
import com.atomist.project.edit.{ProjectEditor, ProjectEditorUtils}
import com.atomist.project.generate.ProjectGenerator
import com.atomist.project.review.ProjectReviewer
import com.atomist.rug.runtime._
import com.atomist.rug.runtime.js.interop.jsPathExpressionEngine
import com.atomist.rug.runtime.plans.{InstructionRunner, LocalPlanRunner, PlanRunner}
import com.atomist.rug.spi.Handlers.Instruction._
import com.atomist.rug.spi.Handlers.Status.{Failure, Success}
import com.atomist.rug.spi.Handlers.{Instruction, Response}
import com.atomist.source.{ArtifactSource, ArtifactSourceLocator}

/**
  * Run instructions synchronously in this JVM
  *
  * TODO - ensure we blow up if there are rugs or instructions with duplicate names
  * and don't have different GA's
  */
class LocalInstructionRunner(rugs: Seq[AddressableRug],
                             teamIdToRunWith: String,
                             projectFinder: ProjectFinder,
                             projectPersister: ProjectPersister,
                             planRunner: PlanRunner,
                             runningPathExpressionEngine: jsPathExpressionEngine)
  extends InstructionRunner {

  private def doWithProjectName(instruction: Instruction, action: (String) => Response) = {
    instruction.detail.projectName match {
      case Some(projectName) => action(projectName)
      case _ => Response(Failure, None, None, Some(s"Project name required for $instruction."))
    }
  }

  private def doWithProject(instruction: Instruction, action: (ArtifactSource) => Response) = {
    doWithProjectName(
      instruction,
      (projectName: String) => {
        projectFinder.findArtifactSource(projectName) match {
          case Some(project) => action(project)
          case _ => Response(Failure, None, None, Some(s"Project '$projectName' could not be found."))
        }
      }
    )
  }

  override def run(instruction: Instruction, callbackInput: AnyRef): Response = {
    val parameters = SimpleParameterValues(instruction.detail.parameters)
    instruction match {
      case Execute(detail) =>
        //TODO execute - look up a JVM object from a registry somewhere, run it, and return the result. Use JVM ServiceLocator stuff. Just like a Rug, but in pure JVM.
        Response(Success, None, None, None)
      case _ =>
        rugs.find(p => matches(p, instruction)) match {
          case Some(rug: ProjectGenerator) =>
            doWithProjectName(instruction, (projectName: String) => {
              val newProject = rug.generate(projectName, parameters)
              val persistAttempt = projectPersister.persist(projectName, newProject)
              Response(Success, None, None, Some(persistAttempt))
            })
          case Some(rug: ProjectEditor) =>
            doWithProject(instruction, (project: ArtifactSource) => {
              val modificationAttempt = rug.modify(project, parameters)
              Response(Success, None, None, Some(modificationAttempt))
            })
          case Some(rug: ProjectReviewer) =>
            doWithProject(instruction, (project: ArtifactSource) => {
              val reviewResult = rug.review(project, parameters)
              Response(Success, None, None, Some(reviewResult))
            })
          case Some(rug: CommandHandler) =>
            object context extends CommandContext{
              override def teamId: String = teamIdToRunWith
              override def pathExpressionEngine = runningPathExpressionEngine
            }
            val planOption = rug.handle(context, parameters)
            val planResult = planOption.map(planRunner.run(_, callbackInput))
            Response(Success, None, None, Some(planResult))

          case Some(rug: ResponseHandler) =>
            callbackInput match {
              case response: InstructionResponse =>
                val planOption = rug.handle(response, parameters)
                val planResult = planOption.map(planRunner.run(_, callbackInput))
                Response(Success, None, None, Some(planResult))
              case _ =>
                Response(Failure, None, None, Some("Callback input was not recognized."))
            }
          case rug => Response(Failure, None, None, Some(s"Cannot execute rug $rug."))
        }
    }
  }

  private def matches(rug: AddressableRug, instruction: Instruction): Boolean = {
    val typeMatches = (instruction, rug) match {
      case (i: Edit, r: ProjectEditor) => true
      case (i: Generate, r: ProjectGenerator) => true
      case (i: Review, r: ProjectReviewer) => true
      case (i: Command, r: CommandHandler) => true
      case (i: Respond, r: ResponseHandler) => true
      case _ => false
    }
    typeMatches && instruction.detail.name == rug.name &&
      instruction.detail.coordinates.exists(coords => rug.group == coords.group && rug.artifact == coords.artifact)
  }
}
trait ProjectFinder {
  def findArtifactSource(project_name: String) : Option[ArtifactSource]
}

trait ProjectPersister {
  def persist(projectName: String, source: ArtifactSource): PersistAttempt
}

sealed trait PersistAttempt
case class FailedToPersist(t: Throwable)
case class PersistSuccess(artifactSourceLocator: ArtifactSourceLocator)
