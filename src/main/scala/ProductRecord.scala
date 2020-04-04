package object codesample {
  type ProductRecord = ProductModule.Record
}

package codesample {

case class Field[+T](name: String) {
  def compute(record: DataRecord)(implicit visited: Seq[Field[Any]]): T =
    record.rawValues(this).asInstanceOf[T]

  final def apply(record: DataRecord)(implicit visited: Seq[Field[Any]] = Seq()): T = {
    if (visited contains this)
      throw new IllegalStateException(
        s"Circular dependency chain while computing $name; visited " +
        (visited :+ this).map(_.name).mkString(" -> "))
    else
      compute(record)(visited :+ this)
  }
}

trait DataRecord {
  def rawValues: Map[Field[Any], Any]
  def apply[T](field: Field[T])(implicit visited: Seq[Field[Any]] = Seq()): T =
    field.apply(this)
}

trait RecordModule {
  def recordName: String

  // Support for meta-functions
  lazy val allFields: Seq[Field[Any]] = {
    this.getClass.getMethods.filter(f =>
      classOf[Field[Any]].isAssignableFrom(f.getReturnType)
    ).map(_.invoke(this).asInstanceOf[Field[Any]])
  }
  lazy val rawFields = allFields.filter(_.getClass == classOf[Field[Any]])
  lazy val derivedFields = allFields.filterNot(rawFields contains _)

  case class Record(rawValues: Map[Field[Any], Any]) extends DataRecord {
    override def toString() = {
      val nameWidth = allFields.map(_.name.length).max + 2

      recordName + ":\n  Raw Fields:\n" +
      rawFields.map(f =>
        String.format(s"    %-${nameWidth}s%s%n", f.name + ":", f(this).toString)
      ).mkString +
      "  Derived Fields:\n" +
      derivedFields.map(f =>
        String.format(s"    %-${nameWidth}s%s%n", f.name + ":", f(this).toString)
      ).mkString +
      "\n"
    }
  }
}

/** Deserialize input into some record structure.
 * Individual formats should be subclasses of this abstract class,
 * implementing the parseRecord method.
 */
abstract trait LineBasedDeserializer[T] {
  def parseRecord(data: String): T

  def parseRecordIterator(data: Iterator[String], sourceName: String = ""): Iterator[T] =
    data.zipWithIndex.flatMap{ case (line, lineNumber) => try {
      Some(parseRecord(line))
    } catch {
      case e: IllegalArgumentException =>
        println(s"$sourceName${lineNumber + 1}: ${e.getMessage}")
        None
    }}

  def parseSource(source: scala.io.Source, sourceName: String = ""): Iterator[T] =
    parseRecordIterator(source.getLines, sourceName)

  def parseFile(filename: String): Iterator[T] =
    parseSource(scala.io.Source.fromFile(filename), filename+":")

  def parse[T](data: String, start: Int, end: Int, field: Field[T])(implicit converter: String => T): (Field[T], T) = {
    field -> converter(data.substring(start, end))
  }

  implicit def stringToInt(s: String) = s.toInt
  implicit def stringToBooleans(s: String): Seq[Boolean] = s.toCharArray.collect {
    case 'Y' => true
    case 'N' => false
    case other => throw new IllegalArgumentException(s"Couldn't parse '$other' as boolean")
  }
}

object ProductModule extends RecordModule {
  val recordName = "ProductRecord"

  // Raw Fields
  val productId                = Field[Int]   ("Product Id")
  val productDescription       = Field[String]("Product Description")
  val regularSingularPrice     = Field[Int]   ("Regular Singular Price")
  val promotionalSingularPrice = Field[Int]   ("Promotional Singular Price")
  val regularSplitPrice        = Field[Int]   ("Regular Split Price")
  val promotionalSplitPrice    = Field[Int]   ("Promotional Split Price")
  val regularForX              = Field[Int]   ("Regular For X")
  val promotionalForX          = Field[Int]   ("Promotional For X")
  val flags                    = Field[Seq[Boolean]]("Flags")
  val productSize              = Field[String]("Product Size")

  // Derived Fields
  val regularDisplayPrice = new Field[String]("Regular Display Price") {
    override def compute(record: DataRecord)(implicit visited: Seq[Field[Any]]) =
      displayPrice(record, regularSingularPrice, regularSplitPrice, regularForX)
  }
  val promotionalDisplayPrice = new Field[String]("Promotional Display Price") {
    override def compute(record: DataRecord)(implicit visited: Seq[Field[Any]]) =
      displayPrice(record, promotionalSingularPrice, promotionalSplitPrice, promotionalForX)
  }
  /* I'm currently representing calculator prices as a long integer in
   * hundredths of a penny.  If the desired precision or scale might ever
   * change, it may be better to use BigDecimal so that the MathContext
   * can be more conveniently adjusted.
   */
  val regularCalculatorPrice = new Field[Long]("Regular Calculator Price") {
    override def compute(record: DataRecord)(implicit visited: Seq[Field[Any]]) =
      calculatorPrice(record, regularSingularPrice, regularSplitPrice, regularForX)
  }
  val promotionalCalculatorPrice = new Field[Long]("Promotional Calculator Price") {
    override def compute(record: DataRecord)(implicit visited: Seq[Field[Any]]) =
      calculatorPrice(record, promotionalSingularPrice, promotionalSplitPrice, promotionalForX)
  }
  val unitOfMeasure = new Field[String]("Unit of Measure") {
    override def compute(record: DataRecord)(implicit visited: Seq[Field[Any]]) =
      if (isPerWeight(record)) "Pound" else "Each"
  }
  val taxRate = new Field[BigDecimal]("Tax Rate") {
    override def compute(record: DataRecord)(implicit visited: Seq[Field[Any]]) =
      if (isTaxable(record)) BigDecimal("7.775") else BigDecimal("0")
  }

  // These give semantic context to individual flags
  val isAgeRestricted = new Field[Boolean]("Is Age Restricted") {
    override def compute(record: DataRecord)(implicit visited: Seq[Field[Any]]) =
      flags(record).apply(0) // inferred from sample data
  }
  val isPerWeight = new Field[Boolean]("Is Per-Weight") {
    override def compute(record: DataRecord)(implicit visited: Seq[Field[Any]]) =
      flags(record).apply(2)
  }
  val isTaxable = new Field[Boolean]("Is Taxable") {
    override def compute(record: DataRecord)(implicit visited: Seq[Field[Any]]) =
      flags(record).apply(4)
  }

  // Support functions for prices
  def priceFormat(cents: Int) =
    s"${if (cents < 0) "-" else ""}$$${cents.abs / 100}.${(cents.abs % 100).formatted("%02d")}"
  def displayPrice(record: DataRecord, singular: Field[Int], split: Field[Int], forX: Field[Int]) = {
    if (singular(record) != 0)
      priceFormat(singular(record))
    else if (split(record) != 0)
      s"${forX(record)} for ${priceFormat(split(record))}"
    else
      ""
  }
  /* I'm currently representing calculator prices as a long integer in
   * hundredths of a penny.  If the desired precision or scale might ever
   * change, it may be better to use BigDecimal so that the MathContext
   * can be more conveniently adjusted.
   */
  def calculatorPrice(record: DataRecord, singular: Field[Int], split: Field[Int], forX: Field[Int]) = {
    if (singular(record) != 0)
      singular(record) * 100L
    else if (split(record) != 0) {
      val count = forX(record)
      // This implements half-down rounding
      (split(record) * 100L + (count - 1) / 2) / count
    } else {
      0L
    }
  }
}
object ProductRecordDeserializer extends LineBasedDeserializer[ProductRecord] {
  def parseRecord(data: String): ProductRecord = {
    import ProductModule._

    Record(Map(
      parse(data,   0,   8, productId),
      parse(data,   9,  68, productDescription)(_.trim),
      parse(data,  69,  77, regularSingularPrice),
      parse(data,  78,  86, promotionalSingularPrice),
      parse(data,  87,  95, regularSplitPrice),
      parse(data,  96, 104, promotionalSplitPrice),
      parse(data, 105, 113, regularForX),
      parse(data, 114, 122, promotionalForX),
      parse(data, 123, 132, flags),
      parse(data, 133, 142, productSize)(_.trim)
    ))
  }
}

}
