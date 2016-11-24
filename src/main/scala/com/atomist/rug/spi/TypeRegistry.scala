package com.atomist.rug.spi

/**
  * Allows extension to Rug functionality by understanding noun
  * kinds, such as "file", "line" or "class"
  */
trait TypeRegistry {

  def findByName(kind: String): Option[Type]

  def kindNames: Traversable[String]

  def kinds: Seq[Type]
}