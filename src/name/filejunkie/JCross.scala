package name.filejunkie

abstract class JCross(descriptionColumns : Seq[Seq[Long]], descriptionRows : Seq[Seq[Long]]){
  protected val xSize = descriptionColumns.size
  protected val ySize = descriptionRows.size

  protected def cells: Seq[Seq[Cell.Value]]

  protected def validate(cells: Seq[Cell.Value], description: Seq[Long]): Boolean = {
    val blocks = findBlocks(cells)

    blocks.equals(description)
  }

  protected def validate(cells: Seq[Seq[Cell.Value]], descriptionColumns: Seq[Seq[Long]], descriptionRows: Seq[Seq[Long]]): Boolean = {
    val cellsTrans = cells.transpose

    !(1 to xSize).exists(i => {
      !validate(cells(i), descriptionColumns(i))
    }) &&
    !(1 to ySize).exists(i => {
      !validate(cellsTrans(i), descriptionRows(i))
    })
  }

  protected def findBlocks(cells: Seq[Cell.Value]) : List[Long] = {
    val firstBlackIdx = cells.indexOf(Cell.Black)

    if(firstBlackIdx == 0){
      val firstNonBlackIds = cells.indexWhere(_ != Cell.Black)

      if(firstNonBlackIds != -1)
        firstNonBlackIds :: findBlocks(cells.drop(firstNonBlackIds))
      else
        cells.length :: Nil
    }
    else if(firstBlackIdx == -1){
      Nil
    }
    else{
      findBlocks(cells.drop(firstBlackIdx))
    }
  }

  override def toString = {
    val sb = new StringBuilder()

    val leftShift =
      descriptionRows.map(x =>
        x.map(_.toString.size).reduceLeft((m: Int, n: Int) => m + n) + x.size
        ).max

    val topElems = descriptionColumns.map(_.size).max

    val cellSize = descriptionColumns.map(_.map(_.toString.size).max).max + 1

    sb ++= headerToString
    sb ++= bodyToString

    // Printing top header
    def headerToString : String = {
      val sb = new StringBuilder()

      (0 to topElems - 1) foreach (i => {
        sb ++= " " * leftShift

        descriptionColumns foreach (colDefList => {
          val idx = colDefList.size - topElems + i

          if (idx >= 0) {
            val v = colDefList(idx)
            sb ++= v.toString

            sb ++= " " * (cellSize - v.toString.size)
          }
          else {
            sb ++= " " * cellSize
          }
        })

        sb += '\n'
      })
      sb.toString
    }

    // Printing everything else
    def bodyToString: String = {
      val sb = new StringBuilder()

      (0 to ySize - 1) foreach (i => {
        val rowDefList = descriptionRows(i)
        val descriptionipt = (rowDefList map (_.toString)).reduceLeft((m: String, n: String) => m + " " + n)

        sb ++= " " * (leftShift - descriptionipt.size)

        sb ++= descriptionipt

        val str = ((0 to xSize - 1) map (j => {
          val cell = cells(j)(i)

          val symbol = cell match {
            case Cell.Black => "X"
            case Cell.White => " "
            case Cell.Maybe => "?"
            case Cell.Unknown => "*"
          }

          Seq.fill(cellSize) {
            symbol
          }.reduceLeft((m: String, n: String) => m + n)
        })).reduceLeft((m: String, n: String) => m + n)

        sb ++= str + '\n'

        sb ++= (" " * leftShift + str + '\n') * (cellSize - 1)

      })

      sb.toString
    }

    sb.toString

  }
}