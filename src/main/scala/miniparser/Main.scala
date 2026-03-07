package miniparser

import java.nio.file.Files
import java.nio.file.Path

object Main:
  def main(args: Array[String]): Unit =
    if args.isEmpty then
      System.err.println("Usage: miniparser.Main <path> [--tokens]")
      System.exit(1)

    val showTokens = args.contains("--tokens")
    val path = Path.of(args.find(_ != "--tokens").getOrElse(args.head))
    val input = Files.readString(path)
    val tokens = Tokenizer.tokenize(input)

    if showTokens then
      tokens.foreach(println)

    val ast = Parser(tokens).parseSourceFile()
    println(ast)
