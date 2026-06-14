package scalanotation.internal

private[scalanotation] object NumericPromotions:
  private[scalanotation] def isExactFloat(value: Int): Boolean =
    value.toFloat.toInt == value
