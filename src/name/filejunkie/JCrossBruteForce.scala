package name.filejunkie

import scala.collection.mutable

class JCrossBruteForce(descriptionColumns : Seq[Seq[Long]], descriptionRows : Seq[Seq[Long]]) extends JCross(descriptionColumns, descriptionRows) {
  protected val cells: Seq[Seq[Cell.Value]] = {
    val columns = generateLines(ySize);

    val filteredColumns = for(i <- 0 until xSize) yield columns.filter(validate(_, descriptionColumns(i)))

    val tables = new IterableTable(filteredColumns)

    tables.find(validate(_)).getOrElse(Nil)
  }

  private def generateLines(length: Int) : List[List[Cell.Value]] = {
    if(length <= 0){
      Nil
    }
    else if(length == 1){
      List(List(Cell.Black), List(Cell.White))
    }
    else{
      val shorterLines = generateLines(length - 1)
      val list1 = for(line <- shorterLines) yield line ::: List(Cell.Black)
      val list2 = for(line <- shorterLines) yield line ::: List(Cell.White)
      list1 ::: list2
    }
  }

  class IterableTable(columns: Seq[Seq[Seq[Cell.Value]]]) extends Iterable[Seq[Seq[Cell.Value]]] {
    private val limits = columns.map(_.size)
    private val indices = mutable.Seq.fill(columns.size){0}
    private var overflow: Boolean = false

    def iterator = new Iterator[Seq[Seq[Cell.Value]]] {
      def hasNext = !overflow

      def next(): Seq[Seq[Cell.Value]] = {

        val res = for(i <- 0 until ySize) yield {
          val possibleColumns = columns(i)
          val currIdx = indices(i)
          possibleColumns(currIdx)
        }

        var over = false
        var i = 0
        while(!over){

          var index = indices(i)
          val limit = limits(i)

          if (index + 1 >= limit) {
            index = 0
            if (i == indices.size - 1) {
              over = true;
              overflow = true
            }
          }
          else {
            index = index + 1
            over = true
          }

          indices(i) = index
          i = i + 1

        }

        res
      }
    }
  }
}
