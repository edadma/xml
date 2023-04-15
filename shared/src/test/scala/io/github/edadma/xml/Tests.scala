package io.github.edadma.xml

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Tests extends AnyFreeSpec with Matchers with Testing:
  "text" in {
    test("asdf") shouldBe """Text(s = "asdf")"""
  }
