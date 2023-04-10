package io.github.edadma.xml

import pprint.pprintln

@main def run(): Unit =
  pprintln(XML(scala.io.Source.fromString("<tag>asdf</tag>")))
