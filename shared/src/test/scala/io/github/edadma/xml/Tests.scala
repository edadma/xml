package io.github.edadma.xml

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Tests extends AnyFreeSpec with Matchers:
  "text" in {
    test("asdf") shouldBe
      """
        |Text(s = "asdf")
        |""".trim.stripMargin
  }

  "text in element" in {
    test("<tag>asdf</tag>") shouldBe
      """
        |Element(name = "tag", attrs = Map(), body = List(Text(s = "asdf")))
        |""".trim.stripMargin
  }

  "text in nested element" in {
    test("""
        |<note>
        |  <to>Tove</to>
        |  <from>Jani</from>
        |  <heading>Reminder</heading>
        |  <body>Don't forget me this weekend!</body>
        |</note>
        |""".stripMargin) shouldBe
      s"""
        |Element(
        |  name = "note",
        |  attrs = Map(),
        |  body = List(
        |    Text(
        |      s = \"\"\"
        |  \"\"\"
        |    ),
        |    Element(name = "to", attrs = Map(), body = List(Text(s = "Tove"))),
        |    Text(
        |      s = \"\"\"
        |  \"\"\"
        |    ),
        |    Element(name = "from", attrs = Map(), body = List(Text(s = "Jani"))),
        |    Text(
        |      s = \"\"\"
        |  \"\"\"
        |    ),
        |    Element(name = "heading", attrs = Map(), body = List(Text(s = "Reminder"))),
        |    Text(
        |      s = \"\"\"
        |  \"\"\"
        |    ),
        |    Element(name = "body", attrs = Map(), body = List(Text(s = "Don't forget me this weekend!"))),
        |    Text(
        |      s = \"\"\"
        |\"\"\"
        |    )
        |  )
        |)
        |""".trim.stripMargin
  }

  "attribute" in {
    test("""
        |<note date="2008-01-10">
        |  <to>Tove</to>
        |  <from>Jani</from>
        |</note>
        |""".stripMargin) shouldBe
      s"""
        |Element(
        |  name = "note",
        |  attrs = Map("date" -> "2008-01-10"),
        |  body = List(
        |    Text(
        |      s = \"\"\"
        |  \"\"\"
        |    ),
        |    Element(name = "to", attrs = Map(), body = List(Text(s = "Tove"))),
        |    Text(
        |      s = \"\"\"
        |  \"\"\"
        |    ),
        |    Element(name = "from", attrs = Map(), body = List(Text(s = "Jani"))),
        |    Text(
        |      s = \"\"\"
        |\"\"\"
        |    )
        |  )
        |)
        |""".trim.stripMargin
  }
