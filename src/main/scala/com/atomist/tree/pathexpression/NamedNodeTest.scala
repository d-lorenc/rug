package com.atomist.tree.pathexpression

import com.atomist.rug.spi.TypeRegistry
import com.atomist.tree.TreeNode
import com.atomist.tree.content.text.TreeNodeOperations
import com.atomist.tree.pathexpression.ExecutionResult.ExecutionResult

/**
  * Follow children of the given name. If not, methods of that name.
  * @param name
  */
case class NamedNodeTest(name: String)
  extends NodeTest {

  private def findUnder(tn: TreeNode): List[TreeNode] = {
    tn.childrenNamed(name).toList match {
      case Nil =>
        TreeNodeOperations.invokeMethodIfPresent[TreeNode](tn, name).
          map {
            case s: List[TreeNode@unchecked] =>
              s
            case t: TreeNode =>
              List(t)
            case x =>
              Nil
          }.getOrElse(Nil)
      case l => l
    }
  }

  override def follow(tn: TreeNode, axis: AxisSpecifier, ee: ExpressionEngine, typeRegistry: TypeRegistry): ExecutionResult = axis match {
    case Child =>
      val kids: List[TreeNode] = findUnder(tn)
      Right(kids)
    case Descendant =>
      val possibleMatches: List[TreeNode] = Descendant.selfAndAllDescendants(tn).flatMap(n => findUnder(n)).toList
      // TODO: not removing duplicates? NodesWithTag does. Pretty sure this should too. need to write a test
      Right(possibleMatches)
  }

}
