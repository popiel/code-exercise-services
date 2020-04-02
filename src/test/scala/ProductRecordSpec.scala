package codesample

import org.scalatest._
import org.scalatest.funspec._
import scala.math.BigDecimal

class ProductRecordSpec extends AnyFunSpec with Matchers {
  describe("Price") {
    describe("display text") {
      it("should omit the count for singular prices") {
        Price(10400, 1).display shouldBe "$1.04"
      }
      it("should include the count for groups") {
        Price(56780, 2).display shouldBe "2 for $5.68"
      }
    }
    describe("calculator value") {
      it("should round to nearest hundredth of a penny") {
        Price((54321 * 3) + 0, 3).calculator shouldBe 54321
        Price((54321 * 3) + 1, 3).calculator shouldBe 54321
        Price((54321 * 3) + 2, 3).calculator shouldBe 54322
      }
      it("should round exact halves down") {
        // Make sure we're not rounding to even
        Price((12345 * 4) + 2, 4).calculator shouldBe 12345
        Price((12346 * 4) + 2, 4).calculator shouldBe 12346
      }
    }
  }

  describe("ProductRecord") {
    val apple = ProductRecord(1, "apple", Price(34900, 1), None, Seq(false, false, true, false, false), "lb")
    val cups = ProductRecord(2, "paper cups", Price(9900, 1), None, Seq(false, false, false, false, true), "20 count")

    it("should have tax iff flag 5 is set") {
      apple.taxRate shouldBe 0
      cups.taxRate should be > BigDecimal(0)
    }

    it("should have unit of measure based on flag 3") {
      apple.unitOfMeasure shouldBe "Pound"
      cups.unitOfMeasure shouldBe "Each"
    }
  }
}
