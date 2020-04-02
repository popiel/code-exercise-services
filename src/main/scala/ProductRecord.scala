package codesample

import scala.util.control.NoStackTrace

/* I'm currently recording prices as a long integer in hundredths of a penny.
 * If the desired precision or scale might ever change, it may be better to
 * use BigDecimal so that the MathContext can be more conveniently adjusted.
 */
case class Price(baseInCentiCents: Long, count: Int) {
  require(count > 0)

  override def toString(): String = {
    (if (count == 1) "" else s"${count} for ") +
    (if (baseInCentiCents < 0) "-" else "") +
    "$" + (baseInCentiCents.abs / 10000).toString +
    "." + ((baseInCentiCents.abs % 10000 + 49) / 100).formatted("%02d")
  }

  /** Human-readable price for group lot */
  def display = toString

  /** Price per single item, rounded half-down,
   * still in hundredths of a penny. */
  def calculator = (baseInCentiCents + ((count - 1) / 2)) / count
}

case class ProductRecord(
  id: Int,
  description: String,
  regularPrice: Price,
  promotionalPrice: Option[Price],
  flags: Seq[Boolean],
  size: String
) {
  // These give semantic context to individual flags
  def isAgeRestricted = flags(0) // inferred from sample data
  def isPerWeight = flags(2)
  def isTaxable = flags(4)

  // The following are all just convenience functions for accessing derivitaves of other fields
  def taxRate = {
    // The spec doesn't say anything about how the tax rate is actually used,
    // so I'm just noting it as a BigDecimal since it will likely be applied
    // against currencies.
    import scala.math.BigDecimal
    if (isTaxable) BigDecimal("0.07775") else BigDecimal("0")
  }
  def unitOfMeasure = if (isPerWeight) "Pound" else "Each"
  def regularDisplayPrice = regularPrice.display
  def regularCalculatorPrice = regularPrice.calculator
  def promotionalDisplayPrice = promotionalPrice.map(_.display)
  def promotionalCalculatorPrice = promotionalPrice.map(_.calculator)
}

/** Deserialize input into some record structure.
 * Individual formats should be subclasses of this abstract class,
 * implementing the parseRecord method.
 */
abstract trait LineBasedDeserializer[T] {
  def parseRecord(data: String): T

  def parseRecordIterator(data: Iterator[String], sourceName: String = ""): Iterator[T] =
    data.zipWithIndex.map{ case (line, lineNumber) => try {
      parseRecord(line)
    } catch {
      case e: IllegalArgumentException =>
        throw new IllegalArgumentException(s"$sourceName${lineNumber + 1}: ${e.getMessage}", e) with NoStackTrace
    }}

  def parseSource(source: scala.io.Source, sourceName: String = ""): Iterator[T] =
    parseRecordIterator(source.getLines, sourceName)

  def parseFile(filename: String): Iterator[T] =
    parseSource(scala.io.Source.fromFile(filename), filename+":")
}

class ProductRecordDeserializer extends LineBasedDeserializer[ProductRecord] {
  // This has a bunch of format error checking in it
  // despite the spec saying I didn't need to do it...
  // I've just tripped over bad formatting too much in my life to ignore it.
  // Plus, it's a good baseline to know that input data isn't completely bogus.

  def parseRecord(data: String): ProductRecord = {
    require(data.length >= 142, "Input line too short")

    def number(start: Int, end: Int, name: String) = {
      try { data.substring(start, end).toInt }
      catch {
        case e: NumberFormatException =>
          throw new IllegalArgumentException(
            s"Couldn't parse number for $name from '${data.substring(start, end)}'", e)
      }
    }

    def price(start: Int, name: String): Option[Price] = {
      val single = number(start, start + 8, s"$name Singular Price")
      val split = number(start + 18, start + 26, s"$name Split Price")
      val forX = number(start + 36, start + 44, s"$name For X")

      if (single == 0 && split == 0) None
      else Some {
        require(single == 0 || split == 0, s"Only one of $name Singular Price or $name Split Price may be specified")
        require(split == 0 || forX > 0, s"$name For X must be greater than 0 when specifying $name Split Price")

        if (single == 0) Price(split.toLong * 100, forX) else Price(single.toLong * 100, 1)
      }
    }

    val id = number(0, 8, "Product Id")
    val description = data.substring(9, 68).trim
    val regularPrice = price(69, "Regular")
    val promotionalPrice = price(78, "Promotional")
    val size = data.substring(133, 142).trim
    val flags = (for (pos <- 1 to 9) yield {
      data.charAt(pos + 122) match {
        case 'Y' => true
        case 'N' => false
        case other => throw new IllegalArgumentException(
          s"Couldn't parse flag $pos from '$other'")
      }
    })

    require(regularPrice.nonEmpty, "One of Regular Singluar Price or Regular Split Price must be specified")

    ProductRecord(id, description, regularPrice.get, promotionalPrice, flags, size)
  }
}

// Convenient singleton
object ProductRecordDeserializer extends ProductRecordDeserializer
