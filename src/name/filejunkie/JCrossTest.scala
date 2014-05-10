package name.filejunkie

object JCrossTest {

  def main(args: Array[String]): Unit = {
    val jCross = new JCrossBruteForce(
      Seq(Seq(2), Seq(2), Seq(4,2), Seq(3,1), Seq(9), Seq(3), Seq(4), Seq(2), Seq(2)),
      Seq(Seq(1), Seq(5), Seq(7), Seq(9), Seq(1,1,1,1,1), Seq(1), Seq(1), Seq(1,1), Seq(3))
    )

    println(jCross)
  }

}