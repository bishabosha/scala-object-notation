package scalanotation

import steps.result.Result

class DedentedStringLiteralSuite extends ScalanotationSuite:
  private val ExperimentalImport =
    "import language.experimental.dedentedStringLiterals"

  private def readExperimentalString(input: String): Result[String, DecodeError] =
    Readers.experimental.readAs[String](s"$ExperimentalImport\n$input")

  private def assertExperimentalString(input: String, expected: String, label: String): Unit =
    assertEquals(readExperimentalString(input), Result.Ok(expected), label)

  private def assertTokenFormat(input: String, expected: String): Unit =
    Readers.experimental.readAs[String](s"$ExperimentalImport\n$input") match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.TokenFormat(expected))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("read experimental dedented string expression"):
    val input =
      s"""$ExperimentalImport
         |'''
         |  i am cow
         |  hear me moo
         |  '''
         |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[String](input),
      Result.Ok("i am cow\nhear me moo")
    )

  test("match Scala 3 run examples for plain dedented strings"):
    val cases = List(
      (
        "noIndent",
        """'''
          |i am cow
          |hear me moo
          |'''""".stripMargin,
        "i am cow\nhear me moo"
      ),
      (
        "withIndentPreserved",
        """'''
          |      i am cow
          |      hear me moo
          |    '''""".stripMargin,
        "  i am cow\n  hear me moo"
      ),
      (
        "empty",
        """'''
          |    '''""".stripMargin,
        ""
      ),
      (
        "singleLine",
        """'''
          |    hello world
          |    '''""".stripMargin,
        "hello world"
      ),
      (
        "blankLines",
        """'''
          |    line 1
          |
          |    line 3
          |    '''""".stripMargin,
        "line 1\n\nline 3"
      ),
      (
        "deepIndent",
        """'''
          |          deeply
          |          indented
          |          content
          |    '''""".stripMargin,
        "      deeply\n      indented\n      content"
      ),
      (
        "mixedIndent",
        """'''
          |      first level
          |        second level
          |          third level
          |    '''""".stripMargin,
        "  first level\n    second level\n      third level"
      ),
      (
        "specialChars",
        """'''
          |    !"#$%&()*+,-./:;<=>?@[\]^_`{|}~
          |    '''""".stripMargin,
        "!\"#$%&()*+,-./:;<=>?@[\\]^_`{|}~"
      ),
      (
        "unicode",
        "'''\n    Hello " + "\u4e16\u754c" + "\n    '''",
        "Hello \u4e16\u754c"
      ),
      (
        "withTabs",
        "'''\n\t\ttab indented\n\t\tcontent here\n\t'''",
        "\ttab indented\n\tcontent here"
      ),
      (
        "emptyLinesAnywhere",
        """'''
          |
          |    content
          |
          |    more content
          |
          |    '''""".stripMargin,
        "\ncontent\n\nmore content\n"
      ),
      (
        "withQuotes",
        """'''
          |    "double quotes"
          |    'single quote'
          |    ''
          |    '''""".stripMargin,
        "\"double quotes\"\n'single quote'\n''"
      )
    )

    cases.foreach { (label, input, expected) =>
      assertExperimentalString(input, expected, label)
    }

  test("experimental import flag is required for dedented strings"):
    val input =
      """'''
        |  i am cow
        |  '''
        |""".stripMargin

    Readers.experimental.readAs[String](input) match
      case Result.Err(error) =>
        error.rootCause match
          case DecodeError.ExpectedType("String", found) =>
            assert(found.startsWith("character literal"))
          case other => fail(s"Expected a String type error, got $other")
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("read declaration with package and experimental import"):
    type Data = (text: String, count: Int)
    val input =
      s"""package foo.bar
         |$ExperimentalImport
         |val data = (
         |  text = '''
         |  hello
         |  world
         |  ''',
         |  count = 2
         |)
         |""".stripMargin

    assertEquals(
      Readers.experimental.readDeclAs[Data](input, rootName = "data", packageName = "foo.bar"),
      Result.Ok((text = "hello\nworld", count = 2))
    )

  test("accept a sequence of experimental imports before expression"):
    val input =
      s"""$ExperimentalImport; $ExperimentalImport
         |'''
         |  line
         |  '''
         |""".stripMargin

    assertEquals(Readers.experimental.readAs[String](input), Result.Ok("line"))

  test("support extended single quote delimiters"):
    val input =
      s"""$ExperimentalImport
         |''''
         |'''
         |text
         |'''
         |''''
         |""".stripMargin

    assertEquals(Readers.experimental.readAs[String](input), Result.Ok("'''\ntext\n'''"))

  test("support five quote delimiters from Scala 3 examples"):
    val input =
      s"""$ExperimentalImport
         |'''''
         |    ''''
         |    content with four quotes
         |    ''''
         |    '''''
         |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[String](input),
      Result.Ok("''''\ncontent with four quotes\n''''")
    )

  test("read dedented strings in vectors"):
    val input =
      s"""$ExperimentalImport
         |Vector(
         |  '''
         |    first
         |  ''',
         |  '''
         |    second
         |  ''',
         |  '''
         |    third
         |  '''
         |)
         |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[Vector[String]](input),
      Result.Ok(Vector("  first", "  second", "  third"))
    )

  test("concatenate dedented string with regular strings"):
    val input =
      s"""$ExperimentalImport
         |"prefix" + '''
         |  middle
         |''' + "suffix"
         |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[String](input),
      Result.Ok("prefix  middlesuffix")
    )

  test("normalize CRLF line endings in dedented strings"):
    val input =
      s"$ExperimentalImport\r\n'''\r\n  alpha\r\n  beta\r\n  '''"

    assertEquals(Readers.experimental.readAs[String](input), Result.Ok("alpha\nbeta"))

  test("reject unsupported experimental imports"):
    val input =
      """import language.experimental.other
        |"ok"
        |""".stripMargin

    Readers.experimental.readAs[String](input) match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.Custom(
            "Unsupported experimental import; only import language.experimental.dedentedStringLiterals is supported"
          )
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("reject dedented string without newline after opening delimiter"):
    val input =
      """'''content
         |  '''
         |""".stripMargin

    assertTokenFormat(
      input,
      "Dedented string literal must start with newline after opening quotes"
    )

  test("reject dedented string with non-whitespace before closing delimiter"):
    val input =
      """'''
         |  content'''
         |""".stripMargin

    assertTokenFormat(
      input,
      "Last line of dedented string literal must contain only whitespace before closing delimiter"
    )

  test("reject dedented string line indented less than closing delimiter"):
    val input =
      """'''
         |content
         |  '''
         |""".stripMargin

    assertTokenFormat(
      input,
      "Line in dedented string literal must be indented at least as much as the closing delimiter"
    )

  test("reject more malformed examples from Scala 3 tests"):
    val lineIndentError =
      "Line in dedented string literal must be indented at least as much as the closing delimiter"
    val closingLineError =
      "Last line of dedented string literal must contain only whitespace before closing delimiter"

    assertTokenFormat(
      """'''     '''
        |""".stripMargin,
      "Dedented string literal must start with newline after opening quotes"
    )
    assertTokenFormat(
      "'''\n\t  content\n    '''",
      lineIndentError
    )
    assertTokenFormat(
      """'''
        |    content
        |    ''''
        |""".stripMargin,
      closingLineError
    )
    assertTokenFormat(
      """'''
        |  some content
        |""".stripMargin,
      "unclosed multi-line string literal"
    )
