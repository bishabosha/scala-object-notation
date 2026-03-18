package scalableconfig

import java.nio.file.Files
import java.nio.file.Path
import scala.collection.immutable.ListMap

import scalanotation.*

import org.virtuslab.yaml.*
import org.virtuslab.yaml.Node
import ujson.Arr
import ujson.Bool
import ujson.Null
import ujson.Num
import ujson.Obj
import ujson.Str
import ujson.Value
import steps.result.Result

object Main:
  def main(args: Array[String]): Unit =
    if args.isEmpty then
      System.err.println(
        "args: <path> --name <name> [--tokens] [--json | --yaml] [--safe-nums]"
      )
      System.exit(1)

    val showTokens   = args.contains("--tokens")
    val exportJson   = args.contains("--json")
    val exportYaml   = args.contains("--yaml")
    val preserveNums = args.contains("--safe-nums")
    if exportJson && exportYaml then
      System.err.println("Error: choose only one of --json or --yaml")
      System.exit(1)

    val path = Path.of(
      args
        .find(arg =>
          arg != "--tokens" && arg != "--json" && arg != "--yaml" && arg != "--safe-nums" && arg != "--name"
        )
        .getOrElse(args.head)
    )
    val nameIdx = args.indexOf("--name")
    if nameIdx == -1 || nameIdx == args.length - 1 then
      System.err.println("Error: --name flag must be followed by a name")
      System.exit(1)
    val name  = args(nameIdx + 1)
    val input = Files.readString(path)

    val ast = Readers.readAs[Expr](input, debugTokens = showTokens) match
      case Result.Ok(value)  => value
      case Result.Err(error) =>
        System.err.println(error.format)
        sys.exit(1)

    render(ast, name, exportJson, exportYaml, preserveNums) match
      case Some(value) => println(value)
      case None        =>
        println(s"Parsed declaration '${name}' successfully")

  private[scalableconfig] def render(
      sourceFile: SourceFile[Expr],
      name: String,
      exportJson: Boolean,
      exportYaml: Boolean,
      preserveNums: Boolean
  ): Option[String] =
    if sourceFile.declaration.name != name then
      throw new IllegalArgumentException(
        s"Expected declaration name '$name' but found '${sourceFile.declaration.name}'"
      )
    val value = sourceFile.declaration.value
    if exportJson then Some(ujson.write(exprToJson(value, preserveNums), indent = 2))
    else if exportYaml then Some(exprToYamlNode(value).asYaml)
    else None

  private def exprToJson(expr: Expr, preserveNums: Boolean): Value =
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        val fields = fieldExprs.map { (name, element) =>
          name -> exprToJson(element, preserveNums)
        }
        Obj.from(fields)
      case Expr.VectorExpr(elements) =>
        Arr.from(elements.map(exprToJson(_, preserveNums)))
      case Expr.StringConstant(value) => Str(value)
      case Expr.CharConstant(value)   => Str(value.toString)
      case Expr.IntConstant(value)    => Num(value.toDouble)
      case Expr.LongConstant(value)   =>
        if preserveNums then Str(s"$value") else Num(value.toDouble)
      case Expr.FloatConstant(value) =>
        if preserveNums then Str(value.toString)
        else if value.isNaN || value.isInfinity then Str(value.toString)
        else Num(value.toDouble)
      case Expr.DoubleConstant(value) =>
        if value.isNaN || value.isInfinity then Str(value.toString)
        else Num(value)
      case Expr.BooleanConstant(value) => Bool(value)
      case Expr.NullConstant           => Null

  private[scalableconfig] def exprToYamlNode(expr: Expr): Node =
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        val fields = fieldExprs.map { (name, element) =>
          Node.ScalarNode(name) -> exprToYamlNode(element)
        }
        Node.MappingNode(ListMap.from(fields))
      case Expr.VectorExpr(elements) =>
        val values = elements.map(exprToYamlNode(_))
        Node.SequenceNode(values*)
      case Expr.StringConstant(value)  => Node.ScalarNode(value)
      case Expr.CharConstant(value)    => Node.ScalarNode(value.toString)
      case Expr.IntConstant(value)     => Node.ScalarNode(value.toString)
      case Expr.LongConstant(value)    => Node.ScalarNode(value.toString)
      case Expr.FloatConstant(value)   => Node.ScalarNode(value.toString)
      case Expr.DoubleConstant(value)  => Node.ScalarNode(value.toString)
      case Expr.BooleanConstant(value) => Node.ScalarNode(value.toString)
      case Expr.NullConstant           => Node.ScalarNode("null")
