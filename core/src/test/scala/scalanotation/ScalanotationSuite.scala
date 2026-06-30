package scalanotation

import munit.FunSuite
import scalanotation.internal.Token
import scalanotation.internal.Tokenizer

abstract class ScalanotationSuite extends FunSuite:
  protected def tokenLabels(input: String): List[String] =
    val tokens = Tokenizer
      .tokenize(input, debug = false)
      .getOrElse(fail(s"Expected tokenization to succeed for input: $input"))
    tokens.map {
      case Token.PackageKw(_)        => "package"
      case Token.ValKw(_)            => "val"
      case Token.VectorId(_)         => "Vector"
      case Token.TrueKw(_)           => "true"
      case Token.FalseKw(_)          => "false"
      case Token.NullKw(_)           => "null"
      case Token.EmptyTupleId(_)     => "EmptyTuple"
      case Token.TupleId(_)          => "Tuple"
      case Token.Keyword(raw, _)     => raw
      case Token.Identifier(name, _) => s"<Identifier:$name>"
      case Token.Equals(_)           => "="
      case Token.Dot(_)              => "."
      case Token.Plus(_)             => "+"
      case Token.Minus(_)            => "-"
      case Token.Comma(_)            => ","
      case Token.Semicolon(_)        => ";"
      case Token.LParen(_)           => "("
      case Token.RParen(_)           => ")"
      case Token.LBracket(_)         => "["
      case Token.RBracket(_)         => "]"
      case Token.Arrow(_)            => "->"
      case Token.Eof(_)              => "eof"
      case token                     =>
        fail(
          s"Unexpected token in test helper: $token\n${tokens.map(t => s"  $t").mkString("\n")}\nfor input: $input"
        )
    }
