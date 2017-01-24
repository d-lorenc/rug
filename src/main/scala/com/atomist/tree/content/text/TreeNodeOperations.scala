package com.atomist.tree.content.text

import com.atomist.tree.content.text.grammar.antlr.EmptyContainerTreeNode
import com.atomist.tree.{ContainerTreeNode, PaddingTreeNode, TerminalTreeNode, TreeNode}
import org.springframework.util.ReflectionUtils

import scala.collection.mutable.ListBuffer

/**
  * Operations on TreeNodes such as tree pruning.
  */
object TreeNodeOperations {

  def invokeMethod[T](n: TreeNode, methodName: String, args: Seq[String]): T = {
    val m = ReflectionUtils.getAllDeclaredMethods(n.getClass).find(m => m.getName.equals(methodName) && m.getParameterCount == args.size).getOrElse(
      throw new IllegalArgumentException(s"Cannot find method with name [$methodName] and ${args.size} parameters on ${n.getClass}")
    )
    // if (!m.getReturnType.equals(classOf[T]) ???
    m.invoke(n, args: _*).asInstanceOf[T]
  }

  def invokeMethodIfPresent[T](n: TreeNode, methodName: String, args: Seq[String] = Nil): Option[T] = {
    ReflectionUtils.getAllDeclaredMethods(n.getClass).find(m => m.getName.equals(methodName) && m.getParameterCount == args.size)
      .map(m => m.invoke(n, args: _*).asInstanceOf[T])
  }

  type NodeTransformer = TreeNode => Option[TreeNode]

  val Identity: NodeTransformer = f => Some(f)

  type TreeOperation = MutableContainerTreeNode => MutableContainerTreeNode

  def treeOperation(ft: NodeTransformer): TreeOperation = mtn => {
    // We put in a new temporary root to ensure that the root node itself gets
    val artificialRoot = new ParsedMutableContainerTreeNode("temporary-artificial-root")
    artificialRoot.appendField(mtn)
    ViewTree(artificialRoot, ft).childNodes.head.asInstanceOf[MutableContainerTreeNode]
  }

  /**
    * Remove collection nodes that merely contain a single collection child.
    * Preserve collection nodes that contain a single terminal child, because
    * that's typically necessary to provide a context for the name of the terminal.
    */
  val Flatten: TreeOperation = treeOperation {
    case ofv: TreeNode if ofv.childNodes.size == 1 =>
      ofv.childNodes.headOption
    case x =>
      Some(x)
  }

  /**
    * Get rid of this level, pulling up its children
    *
    * @param name name of node level to get rid of
    */
  def collapse(name: String): TreeOperation =
    collapse(ctn => ctn.nodeName.equals(name))

  /**
    * Get rid of this level, pulling up its children
    *
    * @param shouldFlatten test for which nodes should be removed
    * @return function from one tree to a partially collapsed tree
    */
  def collapse(shouldFlatten: ContainerTreeNode => Boolean): TreeOperation = treeOperation {
    def filter: NodeTransformer = {
      case ofv: MutableContainerTreeNode =>
        var pulledUp = ListBuffer.empty[TreeNode]
        val kids = ofv.childNodes foreach {
          case ctn: ContainerTreeNode if shouldFlatten(ctn) =>
            pulledUp.appendAll(ctn.childNodes.flatMap(n => filter(n)))
          case x => pulledUp.append(x)
        }
        Some(new ViewTree(ofv, pulledUp))
      case x =>
        Some(x)
    }

    filter
  }

  /**
    * Remove empty container nodes
    */
  val Prune: TreeOperation = treeOperation {
    case ofv: ContainerTreeNode if ofv.childNodes.isEmpty =>
      println(s"Killing empty container ${ofv}")
      None
    case x =>
      Some(x)
  }

  /**
    * TreeOperation that removes padding nodes
    */
  val RemovePadding: TreeOperation = treeOperation {
    case _: PaddingTreeNode =>
      None
    case x =>
      Some(x)
  }

  def removeReservedWordTokens(reservedWords: Set[String]): TreeOperation = treeOperation {
    case tok: TerminalTreeNode if reservedWords.contains(tok.value) =>
      None
    case x =>
      Some(x)
  }

  def removeProductionsNamed(toRemove: Set[String]): TreeOperation = treeOperation {
    case tok: TerminalTreeNode if toRemove.contains(tok.nodeName) =>
      None
    case x =>
      Some(x)
  }

  val Clean: TreeOperation = RemovePadding andThen Prune andThen Flatten

}
