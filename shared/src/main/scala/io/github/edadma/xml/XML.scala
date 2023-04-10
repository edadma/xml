package io.github.edadma.xml

import io.github.edadma.char_reader.CharReader
import io.github.edadma.char_reader.CharReader.EOI

import pprint.pprintln
//  pprintln(XML("""
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
//      |""".stripMargin))

object XML:
  private def skip(r: CharReader): CharReader = if r.ch.isWhitespace then skip(r.next) else r

//  private def skipUntil(r: CharReader): CharReader =
//    r.ch match
//      case EOI => r
//      case

  private def consume(
      r: CharReader,
      until: Char => Boolean,
      buf: StringBuilder = new StringBuilder,
  ): (String, CharReader) =
    r.ch match
      case EOI           => (buf.toString, r)
      case c if until(c) => (buf.toString, r)
      case c =>
        buf += c
        consume(r.next, until, buf)

  def parse(r: CharReader): (XML, CharReader) =
    r.ch match
      // todo: &amp; (&), &lt; (<), &gt; (>), &quot; ("), &apos; (')
      case EOI => r.error("unexpected end of input")
      case '<' =>
        val r0 = skip(r.next)
        val (start, r2) = consume(r0, !_.isLetter)
        val r3 = skip(r2)

        if r3.ch == '/' then
          val r4 = skip(r3.next)

          if r4.ch != '>' then r4.error("expected closing angle bracket of self-closing tag")

          (Element(r0, start, Nil, Nil), r4.next)
        else
          if r3.ch != '>' then r3.error("expected closing angle bracket of start tag")

          val (xml, r4) = parse(r3.next)

          if r4.ch != '<' then r4.error("expected opening angle bracket of end tag")

          val r5 = skip(r4.next)

          if r5.ch != '/' then r4.error("expected slash of end tag")

          val r6 = skip(r5.next)
          val (end, r7) = consume(r6, _ == '>')

          if r7.ch != '>' then r7.error("expected closing angle brack of end tag")
          if start != end then r6.error("start and end tags are not the same")

          (Element(r0, start, Nil, Seq(xml)), r7.next)
        end if
      case _ =>
        val (text, r1) = consume(r, _ == '<')

        (Text(r, text), r1)

  def apply(s: scala.io.Source): XML =
    val (xml, r) = parse(skip(CharReader.fromString(s.mkString)))
    val r1 = skip(r)

    if r1.ch == EOI then xml else r1.error("expected end of input")

abstract class XML:
  val pos: CharReader

case class Element(pos: CharReader, name: String, attrs: Seq[(String, String)], body: Seq[XML]) extends XML
case class Text(pos: CharReader, s: String) extends XML
