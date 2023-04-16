package io.github.edadma.xml

import pprint.pprintln

@main def run(): Unit =
//  pprintln(XML(scala.io.Source.fromString("""
//      |<book id="JHN"><id id="JHN">43-JHN-web.sfm World English Bible (WEB)
//      |</id><ide charset="UTF-8" /><h>John
//      |</h><toc level="1">The Good News According to John
//      |</toc><toc level="2">John
//      |</toc><toc level="3">John
//      |</toc><p sfm="mt" level="2" style="mt2">The Good News According to
//      |</p><p sfm="mt" style="mt1">John
//      |</p><c id="1" />
//      |<p style="p"><v id="1" bcv="JHN.1.1" />
//      |<w s="G1722">In</w>
//      |<w s="G3588">the</w>
//      |<w s="G0746">beginning</w>
//      |<w s="G1510">was</w>
//      |<w s="G3588">the</w>
//      |<w s="G3056">Word</w>,
//      |</p></book>
//      |""".stripMargin)))

//  pprintln(XML(scala.io.Source.fromString(""" <ide charset="UTF-8" /> """)))

  println(test("""
                 |<tag a='b'/>
                 |""".trim.stripMargin))
