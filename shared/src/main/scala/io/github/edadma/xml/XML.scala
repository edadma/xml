package io.github.edadma.xml

import io.github.edadma.char_reader.CharReader
import io.github.edadma.char_reader.CharReader.EOI
import pprint.pprintln

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object XML:
  @tailrec
  private def skip(r: CharReader): CharReader = if r.ch.isWhitespace then skip(r.next) else r

  @tailrec
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

  @tailrec
  private def parseAttributes(
      r: CharReader,
      map: mutable.HashMap[String, String] = new mutable.HashMap,
  ): (Map[String, String], CharReader) =
    val r1 = skip(r)

    if r1.ch.isLetter then
      val (key, r2) = consume(r1, !_.isLetter)
      val r3 = skip(r2)

      if r3.ch != '=' then r3.error("error parsing attribute")

      val r4 = skip(r3.next)

      if r4.ch != '"' then r4.error("error parsing attribute")

      val (value, r5) = consume(r4.next, _ == '"')

      if r5.ch != '"' then r5.error("unclosed attribute value")

      map(key) = value
      parseAttributes(r5.next, map)
    else (map.toMap, r)

  private def parseStartTag(r: CharReader): Option[(CharReader, String, Map[String, String], Boolean, CharReader)] =
    if r.ch == '<' then
      val r1 = skip(r.next)
      val (start, r2) = consume(r1, c => c.isWhitespace || c == '/' || c == '>')
      val (attrs, r3) = parseAttributes(r2)
      val r4 = skip(r3)

      if r4.ch == '/' then
        val r5 = skip(r4.next)

        if r5.ch != '>' then None
        else Some((r1, start, attrs, true, r5.next))
      else if r4.ch != '>' then None
      else Some((r1, start, attrs, false, r4.next))
    else None

  private def parseEndTag(r: CharReader): Option[(CharReader, String, CharReader)] =
    if r.ch == '<' then
      val r1 = skip(r.next)

      if r1.ch != '/' then None
      else
        val r2 = skip(r1.next)
        val (end, r3) = consume(r2, c => c.isWhitespace || c == '>')
        val r4 = skip(r3)

        if r4.ch != '>' then None
        else Some((r2, end, r4.next))
    else None

  @tailrec
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
            if closed then (Element(start, attrs, Nil).pos(r0), r1)
            else
              val (seq, r2) = parseSeq(r1)

              parseEndTag(r2) match
                case None => r2.error("expected end tag")
                case Some((r3, end, r4)) =>
                  if start != end then r3.error(s"start ($start) and end tags are not the same")

                  (Element(start, attrs, seq).pos(r0), r4)
      case _ =>
        val (text, r1) = consume(r, _ == '<')

        (Text(text).pos(r), r1)
  end parse

  def apply(s: scala.io.Source): XML =
    val (xml, r) = parse(skip(CharReader.fromString(s.mkString)))
    val r1 = skip(r)

    if r1.ch == EOI then xml else r1.error("expected end of input")

abstract class XML:
  var pos: CharReader = null

  def pos(p: CharReader): XML =
    pos = p
    this

case class Element(name: String, attrs: Map[String, String], body: Seq[XML]) extends XML
case class Text(s: String) extends XML
