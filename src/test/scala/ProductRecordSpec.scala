package codesample

import org.scalatest._
import org.scalatest.funspec._
import scala.math.BigDecimal

class ProductRecordSpec extends AnyFunSpec with Matchers {
  import ProductModule._
  describe("Price") {
    describe("display text") {
      it("should omit the count for singular prices") {
        val r = Record(Map(regularSingularPrice -> 104))
        r(regularDisplayPrice) shouldBe "$1.04"
      }
      it("should include the count for groups") {
        val r = Record(Map(
          regularSingularPrice -> 0,
          regularSplitPrice -> 568,
          regularForX -> 2
        ))
        r(regularDisplayPrice) shouldBe "2 for $5.68"
      }
      it("should have the negative in front of the dollar for discounts") {
        val r = Record(Map(regularSingularPrice -> -104))
        r(regularDisplayPrice) shouldBe "-$1.04"
      }
      it("should be blank if the price is 0") {
        val r = Record(Map(
          regularSingularPrice -> 0,
          regularSplitPrice -> 0,
          regularForX -> 0
        ))
        r(regularDisplayPrice) shouldBe ""
      }
    }

    describe("calculator value") {
      it("should round to nearest hundredth of a penny") {
        val r = 0 to 2 map { n => Record(Map(
          regularSingularPrice -> 0,
          regularSplitPrice -> (123 + n),
          regularForX -> 3
        ))}
        r(0)(regularCalculatorPrice) shouldBe 4100
        r(1)(regularCalculatorPrice) shouldBe 4133
        r(2)(regularCalculatorPrice) shouldBe 4167
      }
      it("should round exact halves down") {
        val r = 0 to 1 map { n => Record(Map(
          regularSingularPrice -> 0,
          regularSplitPrice -> (150 + n * 100),
          regularForX -> 10000
        ))}
        // Make sure we're not rounding to even
        r(0)(regularCalculatorPrice) shouldBe 1
        r(1)(regularCalculatorPrice) shouldBe 2
      }
    }
  }

  describe("ProductRecord") {
    val apple = Record(Map(flags -> Seq(false, false, true, false, false)))
    val cups = Record(Map(flags -> Seq(false, false, false, false, true)))

    it("should have tax iff flag 5 is set") {
      taxRate(apple) shouldBe 0
      taxRate(cups) should be > BigDecimal(0)
    }

    it("should have unit of measure based on flag 3") {
      apple(unitOfMeasure) shouldBe "Pound"
      cups(unitOfMeasure) shouldBe "Each"
    }
  }

  describe("ProductRecordDeserializer") {
    it("should parse an item with 10 fields correctly") {
      val parsed = ProductRecordDeserializer.parseRecord("80000001 Kimchi-flavored white rice                                  00000567 00000000 00000000 00000000 00000000 00000000 NNNNNNNNN      18oz")
      parsed shouldBe Record(Map(
        productId -> 80000001,
        productDescription -> "Kimchi-flavored white rice",
        regularSingularPrice -> 567,
        regularSplitPrice -> 0,
        regularForX -> 0,
        promotionalSingularPrice -> 0,
        promotionalSplitPrice -> 0,
        promotionalForX -> 0,
        flags -> Seq(false, false, false, false, false, false, false, false, false),
        productSize -> "18oz"
      ))
      parsed(isTaxable) shouldBe false
      parsed(isPerWeight) shouldBe false
      parsed(promotionalDisplayPrice) shouldBe ""
    }
  }
}

