package io.github.edadma.xml

import pprint.*

trait Testing:
  def test(xml: String): String = PPrinter.BlackWhite(XML(scala.io.Source.fromString(xml))).toString // :+ '\n'
