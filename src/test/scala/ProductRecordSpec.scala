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

  describe("ProductRecordDeserializer") {
    it("should parse a non-taxable item with no promo price") {
      val parsed = ProductRecordDeserializer.parseRecord("80000001 Kimchi-flavored white rice                                  00000567 00000000 00000000 00000000 00000000 00000000 NNNNNNNNN      18oz")
      parsed shouldBe ProductRecord(80000001, "Kimchi-flavored white rice", Price(56700, 1), None, Seq(false, false, false, false, false, false, false, false, false), "18oz")
      parsed.isTaxable shouldBe false
    }

    it("should parse a taxable item with promo price") {
      val parsed = ProductRecordDeserializer.parseRecord("14963801 Generic Soda 12-pack                                        00000000 00000549 00001300 00000000 00000002 00000000 NNNNYNNNN   12x12oz")
      parsed shouldBe ProductRecord(14963801, "Generic Soda 12-pack", Price(130000, 2), Some(Price(54900, 1)), Seq(false, false, false, false, true, false, false, false, false), "12x12oz")
      parsed.isTaxable shouldBe true
    }

    it("should complain if the input isn't long enough") {
      val thrown = the [IllegalArgumentException] thrownBy {
        ProductRecordDeserializer.parseRecord("")
      } 
      thrown.getMessage should include ("Input line too short")
    }

    it("should complain if it cannot parse a number") {
      val thrown = the [IllegalArgumentException] thrownBy {
        ProductRecordDeserializer.parseRecord("1x2d34cc Generic Soda 12-pack                                        00000700 00000549 00001300 00000000 00000002 00000000 NNNNYNNNN   12x12oz")
      } 
      thrown.getMessage should include ("Couldn't parse number for Product Id from '1x2d34cc'")
    }

    it("should complain if both singular and split prices are given") {
      val thrown = the [IllegalArgumentException] thrownBy {
        ProductRecordDeserializer.parseRecord("14963801 Generic Soda 12-pack                                        00000700 00000549 00001300 00000000 00000002 00000000 NNNNYNNNN   12x12oz")
      } 
      thrown.getMessage should include ("Only one of Regular Singular Price or Regular Split Price may be specified")
    }
  }
}

