package codesample

object Wrapper {
  def main(args: Array[String]) {
    for {
      filename <- args
      record <- ProductRecordDeserializer.parseFile(filename)
    } {
      println(record)
    }
  }
}
