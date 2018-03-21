
object HW4 {
  def main(args: Array[String]): Unit = {
    val csv = new CSVReader().readCsv("bank.csv")

    for (col <- csv.numericColumns) {
      processColumn(col)
      processNumericColumn(col)
    }
    for (col <- csv.stringColumns) {
      processColumn(col)
    }
  }

  def processNumericColumn(col: Column[Integer]): Unit = {
    val contents = col.contents
    val min = contents.min
    val max = contents.max
    val avg = contents.reduce(_ + _) / col.contents.length
    val out = "min: %s\tmax:%s\taverage:%s " format(min, max, avg)
    println(out)
  }

  def processColumn[T](col: Column[T]): Unit = {
    val contents = col.contents
    val notNulls = contents.count(_ != "")
    val nulls = contents.count(_ == "")
    val nullPercent = nulls / contents.length.doubleValue()

    val uniques = contents.toSet.size
    val mode = contents.toSet.maxBy[Integer](x => contents.count(_ equals x))
    val modePercent = contents.count(_ equals mode) / contents.length.doubleValue()

    val out1 = "not null: %s\t null: %s\t null%%: %3f\t"
      .format(notNulls, nulls, nullPercent)
    val out2 = "uniques:  %s\t mode: %s\t percent equal to mode: %3f\t"
      .format(uniques, mode, modePercent)
    println("--------------------------------------------------------------------")
    println(col.name)
    println(out1)
    println(out2)
  }
}

class CSV(val numericColumns: List[Column[Integer]],
          val stringColumns: List[Column[String]]) {}


class Column[ValueType](val name: String, val contents: List[ValueType]) {}


class CSVReader {
  def readCsv(path: String): CSV = {

    val source = scala.io.Source.fromFile(path)
    val lines = try source.mkString finally source.close()
    val splitLines = lines.split("\n").map(_.split(",")).map(x => x.toList).toList
    val numberOfColumns = splitLines.head.length
    var numColumns = List[Column[Integer]]()
    var strColumns = List[Column[String]]()

    for (colIndex <- 0 until numberOfColumns) {
      var name :: content = splitLines map (_ (colIndex))
      try { // see if this col is integer-able
        Integer.decode(content.head)
        numColumns = new Column(name, content map Integer.decode) :: numColumns
      } catch { // guess it was a string!
        case e: NumberFormatException =>
          strColumns = new Column(name, content) :: strColumns
      }
    }

    new CSV(numColumns, strColumns)
  }
}