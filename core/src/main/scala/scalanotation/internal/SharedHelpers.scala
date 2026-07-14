package scalanotation.internal
import scalanotation.schema.RawSchema

private[scalanotation] trait SharedHelpers:

  private[internal] final def indexOfField(
      fields: IArray[RawSchema.Field],
      name: String
  ): Int =
    var index = 0
    while index < fields.length do
      if fields(index).name == name then return index
      index += 1
    -1

  private[internal] final def missingReadCapability(schema: RawSchema[?]): Nothing =
    throw IllegalStateException(
      s"read is not available for schema ${schema.describeSelf}"
    )

  private[internal] inline def withRead[T, S <: RawSchema[?], R](
      schema: S,
      inline r: S => R | Null
  )(inline f: R => T): T =
    val read = r(schema)
    if read == null then missingReadCapability(schema)
    else f(read.nn)
