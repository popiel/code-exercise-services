package codesample

/** Class for representing fields in our data records.
 * By default, this represents raw fields of a given type,
 * but subclasses can override the <code>compute</code> method
 * to make derived fields based on other data within the record.
 * Computation for derived fields may reference other derived fields,
 * but circular dependencies will cause an IllegalStateException to be thrown.
 */
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

/** Base class for data records. */
trait DataRecord {
  def rawValues: Map[Field[Any], Any]
  def apply[T](field: Field[T])(implicit visited: Seq[Field[Any]] = Seq()): T =
    field.apply(this)
}

/** Abstract base class for describing data records.
 * This will likely be extended by <code>object</code>s,
 * since the descriptions of data records make sense as singletons.
 * When extending, each raw field should just be
 * an instance of <code>Field[T]</code>
 * stored as a <code>val</code> in the class.
 * Each derived fields should be an instance of
 * a subclass of <code>Field[T]</code>
 * which overrides the <code>compute</code> method
 * to do the appropriate computation.
 *
 * The nested <code>Record</code> type represents individual data records.
 * They should be instantiated with a map of all values for their raw fields.
 * Field access is done by either applying
 * the desired <code>Field</code> instance (from the enclosing module)
 * to the record, or applying the record to the field.
 * That is, both <code>record(field)</code> and <code>field(record)</code>
 * work interchangably.
 * Field access for raw fields and derived fields are identical.
 *
 * The values in the raw values map should match
 * the types indicated by their <code>Field[T]</code> keys;
 * while this is not enforced at instantiation,
 * references to the fields will break if it's not obeyed.
 * Similarly, populating all of the raw fields is not enforced,
 * but toString (and possibly derived fields) will explode if some are missing.
 */
trait RecordModule {
  // The pretty name for the record type for toString
  def recordName: String

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

  // Support for meta-functions
  lazy val allFields: Seq[Field[Any]] = {
    this.getClass.getMethods.filter(f =>
      classOf[Field[Any]].isAssignableFrom(f.getReturnType)
    ).map(_.invoke(this).asInstanceOf[Field[Any]])
  }
  lazy val rawFields = allFields.filter(_.getClass == classOf[Field[Any]])
  lazy val derivedFields = allFields.filterNot(rawFields contains _)

}

/** Deserialize input into some record structure.
 * Individual formats should be subclasses of this abstract class,
 * implementing the parseRecord method.
 */
trait LineBasedDeserializer[T] {
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

  /** Utility function for building the raw field value map.
   * This generates a pair suitable for a map entry,
   * based on a substring of the input data
   * converted to the appropriate type for the field.
   */
  def parse[V](data: String, start: Int, end: Int, field: Field[V])(implicit converter: String => V): (Field[V], V) = {
    field -> converter(data.substring(start, end))
  }

  implicit def stringToInt(s: String) = s.toInt
  implicit def stringToBooleans(s: String): Seq[Boolean] = s.toCharArray.collect {
    case 'Y' => true
    case 'N' => false
    case other => throw new IllegalArgumentException(s"Couldn't parse '$other' as boolean")
  }
}
