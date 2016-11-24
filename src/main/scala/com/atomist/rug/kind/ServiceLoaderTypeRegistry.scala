package com.atomist.rug.kind

import com.atomist.rug.RugRuntimeException
import com.atomist.rug.spi._
import com.typesafe.scalalogging.LazyLogging
import _root_.java.util.ServiceLoader
import scala.collection.JavaConverters._

/**
  * Use JDK ServiceLocator to load Type classes. Each
  * JAR files needs a META-INF/services/com.atomist.rug.spi.Type class containing
  * the FQNs of the types it defines.
  *
  * @see Type
  */
class ServiceLoaderTypeRegistry
  extends TypeRegistry with LazyLogging {

  private lazy val typesMap: Map[String, Type] = {
    ServiceLoader.load(classOf[Type]).asScala.map {
      case t: Type =>
        logger.info(s"Registered type extension '${t.name}, with class ${t.getClass},description=${t.description}")
        t.typeInformation match {
          case st: StaticTypeInformation =>
            logger.debug(s"Found operations: ${st.operations.map(st => st.name).mkString(",")}")
          case _ =>
        }
        t.name -> t
      case wtf =>
        throw new RugRuntimeException("ExtensionType", s"Type class ${wtf.getClass} must implement Type interface", null)
    }
  }.toMap

  override def findByName(kind: String): Option[Type] =
    typesMap.get(kind)

  override def kindNames: Traversable[String] = typesMap.keys

  override def kinds: Seq[Type] = typesMap.values.toSeq

}