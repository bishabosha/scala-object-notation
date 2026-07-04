package scalanotation.internal

private[scalanotation] object IdentifierSyntax:
  /** chars above this need the character-data table lookups; at or below it the explicit ASCII
    * ranges decide
    */
  private[internal] inline val MaxAscii = 127

  private val hardKeywords: Set[String] =
    Set(
      "abstract",
      "case",
      "catch",
      "class",
      "def",
      "do",
      "else",
      "enum",
      "export",
      "extends",
      "false",
      "final",
      "finally",
      "for",
      "given",
      "if",
      "implicit",
      "import",
      "lazy",
      "match",
      "new",
      "null",
      "object",
      "override",
      "package",
      "private",
      "protected",
      "return",
      "sealed",
      "super",
      "then",
      "throw",
      "trait",
      "true",
      "try",
      "type",
      "val",
      "var",
      "while",
      "with",
      "yield",
      ":",
      "=",
      "<-",
      "=>",
      "<:",
      ">:",
      "#",
      "@",
      "=>>",
      "?=>"
    )

  private val hexDigits = "0123456789abcdef"

  def isHardKeyword(name: String): Boolean =
    hardKeywords.contains(name)

  def appendIdentifier(name: String, out: ExprRenderer.Output): Unit =
    if isPlainIdentifier(name) && !isHardKeyword(name) then out.append(name)
    else appendQuotedIdentifier(name, out)

  def appendQualifiedIdentifier(name: String, out: ExprRenderer.Output): Unit =
    var start = 0
    var first = true
    while start <= name.length do
      val end = name.indexOf('.', start) match
        case -1    => name.length
        case index => index
      if first then first = false
      else out.append('.')
      appendIdentifier(name.substring(start, end), out)
      start = end + 1

  def isPlainIdentifier(name: String): Boolean =
    if name.isEmpty then false
    else isAlphaIdentifier(name) || isOperatorIdentifier(name)

  private def isAlphaIdentifier(name: String): Boolean =
    if !isIdentifierStart(name.charAt(0)) then false
    else
      var index = 1
      while index < name.length && isIdentifierPart(name.charAt(index)) do index += 1
      if index == name.length then true
      else if index > 0 && name.charAt(index - 1) == '_' && isOperatorPart(name.charAt(index))
      then
        while index < name.length && isOperatorPart(name.charAt(index)) do index += 1
        index == name.length
      else false

  private def isOperatorIdentifier(name: String): Boolean =
    var index = 0
    while index < name.length && isOperatorPart(name.charAt(index)) do index += 1
    index == name.length

  private[internal] def isIdentifierStart(ch: Char): Boolean =
    // ASCII fast path first: Character.isLetter costs a character-data table lookup per char
    (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_' || ch == '$'
      || (ch > MaxAscii && Character.isLetter(ch))

  private[internal] def isIdentifierPart(ch: Char): Boolean =
    (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9')
      || ch == '_' || ch == '$'
      || (ch > MaxAscii && (Character.isLetter(ch) || Character.isDigit(ch)))

  private[internal] def isOperatorPart(ch: Char): Boolean =
    ch match
      case '~' | '!' | '@' | '#' | '%' | '^' | '*' | '+' | '-' | '<' | '>' | '?' | ':' | '=' | '&' |
          '|' | '\\' | '/' =>
        true
      case _ => false

  private def isScalaLetter(ch: Char): Boolean =
    ch.isLetter || ch == '_' || ch == '$'

  private def appendQuotedIdentifier(name: String, out: ExprRenderer.Output): Unit =
    out.append('`')
    var index = 0
    while index < name.length do
      name.charAt(index) match
        case '`'                              => appendUnicodeEscape('`', out)
        case '\n'                             => out.append("\\n")
        case '\r'                             => out.append("\\r")
        case '\t'                             => out.append("\\t")
        case '\b'                             => out.append("\\b")
        case '\f'                             => out.append("\\f")
        case '\\'                             => out.append("\\\\")
        case ch if Character.isISOControl(ch) =>
          appendUnicodeEscape(ch, out)
        case ch =>
          out.append(ch)
      index += 1
    out.append('`')

  private def appendUnicodeEscape(ch: Char, out: ExprRenderer.Output): Unit =
    out.append("\\u")
    out.append(hexDigits.charAt((ch >> 12) & 0xf))
    out.append(hexDigits.charAt((ch >> 8) & 0xf))
    out.append(hexDigits.charAt((ch >> 4) & 0xf))
    out.append(hexDigits.charAt(ch & 0xf))
