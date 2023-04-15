package io.github.edadma.xml

import pprint.*

def test(xml: String): String = PPrinter.BlackWhite(XML(scala.io.Source.fromString(xml))).toString :+ '\n'
