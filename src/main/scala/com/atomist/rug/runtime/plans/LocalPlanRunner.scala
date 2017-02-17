package com.atomist.rug.runtime.plans

import com.atomist.rug.spi.Handlers.Instruction.Respond
import com.atomist.rug.spi.Handlers.Status.{Failure, Success}
import com.atomist.rug.spi.Handlers._

import scala.util.{Try, Failure => ScalaFailure, Success => ScalaSuccess}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Runs Plans in this JVM - i.e. no work distribution.
  * @param messageDeliverer
  * @param instructionExecutor
  * @param nestedPlanExecutor
  */
class LocalPlanRunner(messageDeliverer: MessageDeliverer,
                      instructionRunner: InstructionPlanner,
                      nestedRunner: Option[PlanRunner] = None) extends PlanRunner {

  override def run(plan: Plan, callbackInput: AnyRef): Future[PlanResult] = {
    val messageLog: Seq[MessageDeliveryError] = plan.messages.flatMap { message =>
      Try(messageDeliverer.deliver(message, callbackInput)) match {
        case ScalaFailure(e) =>  Some(MessageDeliveryError(message, e))
        case ScalaSuccess(_) => None
      }
    }
    val instructionResponseFutures: Seq[Future[Iterable[PlanLogEvent]]] = plan.instructions.map { respondable =>
      Future {
        Try { instructionExecutor.run(respondable.instruction, callbackInput) } match {
          case ScalaFailure(t) =>
            Seq(InstructionError(respondable.instruction, t))
          case ScalaSuccess(response) =>

            val callbackOption = response match {
              case Response(Success, _, _, _) => respondable.onSuccess
              case Response(Failure, _, _, _) => respondable.onFailure
            }
            val callbackResultOption: Option[PlanLogEvent] = callbackOption.flatMap { callback =>
              Try(handleCallback(callback, response.body)) match {
                case ScalaFailure(error) =>
                  Some(CallbackError(callback, error))
                case ScalaSuccess(nestedPlanExecutionOption) =>
                  nestedPlanExecutionOption
              }
            }
            Seq(
              Some(InstructionResponse(respondable.instruction, response)),
              callbackResultOption
            ).flatten
        }
      }
    }
    val futureInstructionLog: Future[Seq[PlanLogEvent]] = Future.fold(instructionResponseFutures)(Seq[PlanLogEvent]())(_ ++ _)
    futureInstructionLog.map(instructionLogEvents => PlanResult(messageLog ++ instructionLogEvents))
  }

  private def handleCallback(callback: Callback, instructionResult: Option[AnyRef]): Option[PlanLogEvent] = callback match {
    case m: Message =>
      messageDeliverer.deliver(m, instructionResult.orNull)
      None
    case r: Respond =>
      instructionExecutor.run(r, instructionResult.orNull)
      None
    case p: Plan =>
      val planRunner = nestedRunner.getOrElse(new LocalPlanRunner(messageDeliverer, instructionRunner, nestedRunner))
      val planResult = planRunner.run(p, instructionResult.orNull)
      Some(NestedPlanRun(p, planResult))
  }

}
