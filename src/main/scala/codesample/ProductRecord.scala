package codesample

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
