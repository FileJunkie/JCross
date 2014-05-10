package name.filejunkie

class JCrossBruteForce(descriptionColumns : Seq[Seq[Long]], descriptionRows : Seq[Seq[Long]]) extends JCross(descriptionColumns, descriptionRows) {
  protected val cells: Seq[Seq[Cell.Value]] = {
    val columns = generateLines(ySize);


    Seq.fill(xSize){Seq.fill(ySize){Cell.Unknown}}
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
}
