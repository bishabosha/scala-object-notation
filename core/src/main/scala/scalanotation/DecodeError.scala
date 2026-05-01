package scalanotation

import scala.annotation.tailrec
import DecodeError.Span

enum DecodeError:
  case TokenFormat(message: String)
  case ExpectedExpression(found: String)
  case ExpectedType(expected: String, found: String)
  case UnitValueNotAllowed()
  case ExpectedEquals(found: String)
  case ExpectedRParen(found: String)
  case ExpectedFieldName(found: String)
  case ExpectedVal(found: String)
  case ExpectedPackage(found: String)
  case ExpectedIdentifier(found: String)
  case ExpectedEof(found: String)
  case FieldCountMismatch(expected: Int, actual: Int)
  case FieldOrderMismatch(expected: String, actual: String)
  case MissingField(fieldName: String)
  case UnexpectedField(fieldName: String)
  case UnexpectedRoot(rootName: String)
  case UnexpectedPackage(packageName: String)
  case DuplicateField(fieldName: String)
  case DuplicateSchemaField(fieldName: String)
  case Custom(message: String)
  case AtPath(cause: DecodeError, segment: String)
  case AtToken(cause: DecodeError, tokenSpan: Span)

  def atPath(segment: String): DecodeError = DecodeError.AtPath(this, segment)
  def atToken(span: Span): DecodeError     = DecodeError.AtToken(this, span)

  def path: List[String] =
    this match
      case DecodeError.AtPath(cause, segment) => segment :: cause.path
      case DecodeError.AtToken(cause, _)      => cause.path
      case _                                  => Nil

  def span: Option[Span] =
    this match
      case DecodeError.AtToken(cause, tokenSpan) => cause.span.orElse(Some(tokenSpan))
      case DecodeError.AtPath(cause, _)          => cause.span
      case _                                     => None

  def rootCause: DecodeError =
    this match
      case DecodeError.AtPath(cause, _)  => cause.rootCause
      case DecodeError.AtToken(cause, _) => cause.rootCause
      case other                         => other

  def format: String = {
    @tailrec
    def baseMessage(error: DecodeError): String = error match {
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
      case DecodeError.ExpectedPackage(found) =>
        s"Expected 'package' but found ${found}"
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
      case DecodeError.UnexpectedPackage(packageName) =>
        s"Unexpected package statement '$packageName'"
      case DecodeError.DuplicateField(fieldName) =>
        s"Duplicate field '$fieldName'"
      case DecodeError.DuplicateSchemaField(fieldName) =>
        s"Duplicate field '$fieldName' in schema"
      case DecodeError.Custom(message) =>
        message
      case DecodeError.AtPath(cause, _) =>
        baseMessage(cause)
      case DecodeError.AtToken(cause, _) =>
        baseMessage(cause)
    }

    val finalSpan =
      span.map(span => Seq(s"${span.line}", ":", s"${span.column}", ": ")).getOrElse(Nil)
    val pathStr =
      val base = path
      if base.isEmpty then Nil
      else "In path '" +: base :+ "': "
    val msg = baseMessage(rootCause)
    (finalSpan ++: pathStr :+ msg).mkString
  }

object DecodeError:
  final case class Span(offset: Int, line: Int, column: Int)
