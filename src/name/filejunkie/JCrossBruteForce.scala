package name.filejunkie

class JCrossBruteForce(descriptionColumns : Seq[Seq[Long]], descriptionRows : Seq[Seq[Long]]) extends JCross(descriptionColumns, descriptionRows) {
  protected val cells: Seq[Seq[Cell.Value]] = Seq.fill(xSize){Seq.fill(ySize){Cell.Unknown}}
}
