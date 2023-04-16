package io.github.edadma.xml

import io.github.edadma.char_reader.CharReader
import io.github.edadma.char_reader.CharReader.EOI

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

    if r1.ch.isLetter || r1.ch == '_' then
      val (key, r2) = consume(r1, c => !(c.isLetterOrDigit || c == '_' || c == '-' || c == '.'))
      val r3 = skip(r2)

      if r3.ch != '=' then r3.error("expected a '=' while parsing attribute")

      val r4 = skip(r3.next)
      val delim = r4.ch

      if delim != '"' && delim != '\'' then r4.error("expected a single or double quote while parsing attribute")

      val (value, r5) = consume(r4.next, _ == delim)

      if r5.ch != delim then r5.error("unclosed attribute value")
      if map contains key then r1.error("duplicate key name")

      map(key) = value
      parseAttributes(r5.next, map)
    else (map.toMap, r)

  private def parseProlog(r: CharReader): Option[(CharReader, Map[String, String], CharReader)] =
    if r.ch != '<' then None
    else
      val r0 = r.next

      if r0.ch != '?' then None
      else
        val r1 = skip(r0.next)

        if !r1.ch.isLetter && r1.ch != '_' then
          r1.error("expected a letter or an underscore as the first character of a tag name")

        val (start, r2) = consume(r1, c => c.isWhitespace || c == '?')

        if start != "xml" then None
        else
          val (attrs, r3) = parseAttributes(r2)
          val r4 = skip(r3)

          if r4.ch != '?' then None
          else
            val r5 = r4.next

            if r5.ch != '>' then None
            else Some((r1, attrs, r5.next))

  private def parseStartTag(r: CharReader): Option[(CharReader, String, Map[String, String], Boolean, CharReader)] =
    if r.ch == '<' then
      val r1 = skip(r.next)

      if !r1.ch.isLetter && r1.ch != '_' then
        r1.error("expected a letter or an underscore as the first character of a tag name")

      val (start, r2) = consume(r1, c => c.isWhitespace || c == '/' || c == '>')

      if start.toLowerCase startsWith "xml" then r1.error("a tag name may not begin with the letters 'xml'")

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
    val r = CharReader.fromString(s.mkString)
    val (_, p, r1) = parseProlog(r).getOrElse((null, Map(), r))
    val (xml, r2) = parse(skip(r1))
    val r3 = skip(r2)

    if r3.ch == EOI then xml.prolog(p) else r3.error("expected end of input")

abstract class XML:
  var pos: CharReader = null
  var prolog: Map[String, String] = Map()

  def pos(p: CharReader): XML =
    pos = p
    this

  def prolog(p: Map[String, String]): XML =
    prolog = p
    this

case class Element(name: String, attrs: Map[String, String], body: Seq[XML]) extends XML
case class Text(s: String) extends XML
