package miniparser

import java.nio.file.Files
import java.nio.file.Path
import ujson.Arr
import ujson.Bool
import ujson.Null
import ujson.Num
import ujson.Obj
import ujson.Str
import ujson.Value

object Main:
  def main(args: Array[String]): Unit =
    if args.isEmpty then
      System.err.println("Usage: miniparser.Main <path> --name <name> [--tokens] [--json]")
      System.exit(1)

    val showTokens = args.contains("--tokens")
    val exportJson = args.contains("--json")
    val preserveNums = args.contains("--safe-nums")
    val path = Path.of(args.find(arg => arg != "--tokens" && arg != "--json" && arg != "--safe-nums" && arg != "--name").getOrElse(args.head))
    val nameIdx = args.indexOf("--name")
    if nameIdx == -1 || nameIdx == args.length - 1 then
      System.err.println("Error: --name flag must be followed by a name")
      System.exit(1)
    val name = args(nameIdx + 1)
    val input = Files.readString(path)
    val tokens = Tokenizer.tokenize(input)

    if showTokens then
      tokens.foreach(println)

    val ast = Parser(tokens).parseSourceFile()
    println(render(ast, name, exportJson, preserveNums))

  private[miniparser] def render(sourceFile: SourceFile, name: String, exportJson: Boolean, preserveNums: Boolean): String =
    if sourceFile.declaration.name != name then
      throw new IllegalArgumentException(s"Expected declaration name '$name' but found '${sourceFile.declaration.name}'")
    val value = sourceFile.declaration.value
    if exportJson then ujson.write(exprToJson(value, preserveNums), indent = 2)
    else value.toString

  private def exprToJson(expr: Expr, preserveNums: Boolean): Value =
    expr match
      case Expr.NamedTupleExpr(names, elements) =>
        val fields = Vector.newBuilder[(String, Value)]
        var index = 0
        while index < names.length do
          fields += names(index) -> exprToJson(elements(index), preserveNums)
          index += 1
        Obj.from(fields.result())
      case Expr.VectorExpr(elements) =>
        val values = Vector.newBuilder[Value]
        var index = 0
        while index < elements.length do
          values += exprToJson(elements(index), preserveNums)
          index += 1
        Arr.from(values.result())
      case Expr.StringConstant(value) => Str(value)
      case Expr.CharConstant(value) => Str(value.toString)
      case Expr.IntConstant(value) => Num(value.toDouble)
      case Expr.LongConstant(value) => if preserveNums then Str(s"$value") else Num(value.toDouble)
      case Expr.FloatConstant(value) => if preserveNums then Str(value.toString) else if value.isNaN || value.isInfinity then Str(value.toString) else Num(value.toDouble)
      case Expr.DoubleConstant(value) => if value.isNaN || value.isInfinity then Str(value.toString) else Num(value)
      case Expr.BooleanConstant(value) => Bool(value)
      case Expr.NullConstant => Null
