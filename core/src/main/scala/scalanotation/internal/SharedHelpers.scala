package scalanotation.internal

trait SharedHelpers:
  private[internal] final def missingReadCapability(schema: RawSchema): Nothing =
    throw IllegalStateException(
      s"read is not available for schema ${schema.describeSelf}"
    )

  private[internal] inline def withRead[T, S <: RawSchema, R](
      schema: S,
      inline r: S => R | Null
  )(inline f: R => T): T =
    val read = r(schema)
    if read == null then missingReadCapability(schema)
    else f(read.nn)
