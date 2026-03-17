package scalanotation

enum DecodeError:
  case TokenFormat(message: String)
  case ExpectedExpression(found: String)
  case ExpectedType(expected: String, found: String)
  case UnitValueNotAllowed()
  case ExpectedEquals(found: String)
  case ExpectedRParen(found: String)
  case ExpectedFieldName(found: String)
  case ExpectedVal(found: String)
  case ExpectedIdentifier(found: String)
  case ExpectedEof(found: String)
  case FieldCountMismatch(expected: Int, actual: Int)
  case FieldOrderMismatch(expected: String, actual: String)
  case MissingField(fieldName: String)
  case UnexpectedField(fieldName: String)
  case UnexpectedRoot(rootName: String)
  case DuplicateField(fieldName: String)
  case Custom(message: String)
  case AtPath(segment: String, cause: DecodeError)
  case AtToken(tokenSpan: Span, cause: DecodeError)

  def atPath(segment: String): DecodeError = DecodeError.AtPath(segment, this)
  def atToken(span: Span): DecodeError     = DecodeError.AtToken(span, this)

  def path: List[String] =
    this match
      case DecodeError.AtPath(segment, cause) => segment :: cause.path
      case DecodeError.AtToken(_, cause)      => cause.path
      case _                                  => Nil

  def span: Option[Span] =
    this match
      case DecodeError.AtToken(tokenSpan, _) => Some(tokenSpan)
      case DecodeError.AtPath(_, cause)      => cause.span
      case _                                 => None

  def rootCause: DecodeError =
    this match
      case DecodeError.AtPath(_, cause)  => cause.rootCause
      case DecodeError.AtToken(_, cause) => cause.rootCause
      case other                         => other

  def format: String =
    this match
      case TokenFormat(message)                      => message
      case DecodeError.ExpectedType(expected, found) =>
        s"Expected $expected but found ${found}"
      case DecodeError.ExpectedExpression(found) =>
        s"Expected an expression but found ${found}"
      case DecodeError.UnitValueNotAllowed() => "Unit value '()' is not valid."
      case DecodeError.ExpectedEquals(found) =>
        s"Expected '=' but found ${found}"
      case DecodeError.ExpectedRParen(found) =>
        s"Expected ')' but found ${found}"
      case DecodeError.ExpectedFieldName(found) =>
        s"expected field name 'x = ' but found ${found}"
      case DecodeError.ExpectedVal(found) =>
        s"Expected 'val' but found ${found}"
      case DecodeError.ExpectedIdentifier(found) =>
        s"Expected an identifier but found ${found}"
      case DecodeError.ExpectedEof(found) =>
        s"Expected end of input but found ${found}"
      case DecodeError.FieldCountMismatch(expected, actual) =>
        s"Expected $expected fields but found $actual"
      case DecodeError.FieldOrderMismatch(expected, actual) =>
        s"Field was expected to be '$expected' but was '$actual'"
      case DecodeError.MissingField(fieldName) =>
        s"Missing required field '$fieldName'"
      case DecodeError.UnexpectedField(fieldName) =>
        s"Unexpected field '$fieldName'"
      case DecodeError.UnexpectedRoot(rootName) =>
        s"Unexpected root declaration '$rootName'"
      case DecodeError.DuplicateField(fieldName) =>
        s"Duplicate field '$fieldName'"
      case DecodeError.Custom(message) =>
        message
      case DecodeError.AtPath(segment, cause) =>
        def loop(cause: DecodeError, acc: List[String]): String =
          cause match
            case DecodeError.AtPath(segment, innerCause) =>
              loop(innerCause, segment :: acc)
            case DecodeError.AtToken(tokenSpan, innerCause) =>
              s"${tokenSpan.line}:${tokenSpan.column}: In path '${acc.reverseIterator.mkString}': ${innerCause.format}"
            case other =>
              s"In path '${acc.reverseIterator.mkString}': ${other.format}"

        loop(cause, List(segment))
      case DecodeError.AtToken(tokenSpan, cause) =>
        s"${tokenSpan.line}:${tokenSpan.column}: ${cause.format}"
