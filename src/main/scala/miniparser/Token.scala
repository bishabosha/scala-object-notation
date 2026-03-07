package miniparser

final case class Span(offset: Int, line: Int, column: Int)

enum Token:
  case ValKw(span0: Span)
  case VectorId(span0: Span)
  case TrueKw(span0: Span)
  case FalseKw(span0: Span)
  case NullKw(span0: Span)
  case Identifier(name: String, span0: Span)
  case IntLit(raw: String, value: Int, span0: Span)
  case LongLit(raw: String, value: Long, span0: Span)
  case FloatLit(raw: String, value: Float, span0: Span)
  case DoubleLit(raw: String, value: Double, span0: Span)
  case StringLit(raw: String, value: String, span0: Span)
  case CharLit(raw: String, value: Char, span0: Span)
  case Equals(span0: Span)
  case Plus(span0: Span)
  case Minus(span0: Span)
  case Comma(span0: Span)
  case LParen(span0: Span)
  case RParen(span0: Span)
  case Eof(span0: Span)

  def span: Span = this match
    case ValKw(span) => span
    case VectorId(span) => span
    case TrueKw(span) => span
    case FalseKw(span) => span
    case NullKw(span) => span
    case Identifier(_, span) => span
    case IntLit(_, _, span) => span
    case LongLit(_, _, span) => span
    case FloatLit(_, _, span) => span
    case DoubleLit(_, _, span) => span
    case StringLit(_, _, span) => span
    case CharLit(_, _, span) => span
    case Equals(span) => span
    case Plus(span) => span
    case Minus(span) => span
    case Comma(span) => span
    case LParen(span) => span
    case RParen(span) => span
    case Eof(span) => span
