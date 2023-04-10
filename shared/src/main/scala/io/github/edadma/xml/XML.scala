package io.github.edadma.xml

import io.github.edadma.char_reader.CharReader
import io.github.edadma.char_reader.CharReader.EOI
import pprint.pprintln

import scala.collection.mutable.ListBuffer
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

  private def parseStartTag(r: CharReader): Option[(CharReader, String, Seq[(String, String)], Boolean, CharReader)] =
    if r.ch == '<' then
      val r1 = skip(r.next)
      val (start, r2) = consume(r1, c => c == '/' || c == '>')
      val r3 = skip(r2)

      if r3.ch == '/' then
        val r4 = skip(r2.next)

        if r4.ch != '>' then None
        else Some((r1, start, Nil, true, r4.next))
      else if r2.ch != '>' then None
      else Some((r1, start, Nil, false, r2.next))
    else None

  private def parseEndTag(r: CharReader): Option[(CharReader, String, CharReader)] =
    if r.ch == '<' then
      val r1 = skip(r.next)

      if r1.ch != '/' then None
      else
        val r2 = skip(r1.next)
        val (end, r3) = consume(r2, _ == '>')

        if r3.ch != '>' then None
        else Some((r2, end, r3.next))
    else None

  private def parseSeq(r: CharReader, buf: ListBuffer[XML] = new ListBuffer): (Seq[XML], CharReader) =
    if r.ch == EOI || parseEndTag(r).isDefined then (buf.toList, r)
    else
      val (xml, r1) = parse(r)

      buf += xml
      parseSeq(r1, buf)

  def parse(r: CharReader): (XML, CharReader) =
    r.ch match
      // todo: &amp; (&), &lt; (<), &gt; (>), &quot; ("), &apos; (')
      case EOI => r.error("unexpected end of input")
      case '<' =>
        parseStartTag(r) match
          case None => r.error("error parsing start tag")
          case Some(r0, start, attrs, closed, r1) =>
            if closed then
              (Element(r0, start, Nil, Nil), r1)
            else
              val (seq, r2) = parseSeq(r1)

              parseEndTag(r2) match
                case None => r2.error("expected end tag")
                case Some((r3, end, r4)) =>
                  if start != end then r3.error(s"start ($start) and end tags are not the same")

                  (Element(r0, start, Nil, seq), r4)
      case _ =>
        val (text, r1) = consume(r, _ == '<')

        (Text(r, text), r1)
  end parse

  def apply(s: scala.io.Source): XML =
    val (xml, r) = parse(skip(CharReader.fromString(s.mkString)))
    val r1 = skip(r)

    if r1.ch == EOI then xml else r1.error("expected end of input")

abstract class XML:
  val pos: CharReader

case class Element(pos: CharReader, name: String, attrs: Seq[(String, String)], body: Seq[XML]) extends XML
case class Text(pos: CharReader, s: String) extends XML
