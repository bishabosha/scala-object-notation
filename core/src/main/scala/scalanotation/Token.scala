package scalanotation

final case class Span(offset: Int, line: Int, column: Int)

private[scalanotation] enum Token:
  case ValKw(span: Span)
  case VectorId(span: Span)
  case TrueKw(span: Span)
  case FalseKw(span: Span)
  case NullKw(span: Span)
  case Identifier(name: String, span: Span)
  case IntLit(raw: String, value: Int, span: Span)
  case LongLit(raw: String, value: Long, span: Span)
  case FloatLit(raw: String, value: Float, span: Span)
  case DoubleLit(raw: String, value: Double, span: Span)
  case StringLit(raw: String, value: String, span: Span)
  case CharLit(raw: String, value: Char, span: Span)
  case Equals(span: Span)
  case Plus(span: Span)
  case Minus(span: Span)
  case Comma(span: Span)
  case LParen(span: Span)
  case RParen(span: Span)
  case Eof(span: Span)

  def span: Span

private[scalanotation] object Token:
  lazy val Empty: Token = Token.Eof(Span(0, 0, 0))

  private[scalanotation] given DefaultToken: Internal.HasDefault[Token]:
    val Default: Token = Token.Empty
