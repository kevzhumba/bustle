import ast.{ArithConst, ArithmeticManipulation, ArithmeticManipulationValue}

@main def main(args: String*): Unit = {
  val language = ArithmeticManipulation
  val input: List[List[ArithmeticManipulationValue]] = List(
    List(ArithConst(3), ArithConst(5)),
    List(ArithConst(2), ArithConst(6)),
    List(ArithConst(9), ArithConst(2)),
    List(ArithConst(10), ArithConst(8)),
    List(ArithConst(1), ArithConst(9)),
    List(ArithConst(20), ArithConst(20)),
    List(ArithConst(3), ArithConst(4)),
    List(ArithConst(25), ArithConst(40))
    )
  val outputs: List[ArithmeticManipulationValue] = List(
    ArithConst(5),
    ArithConst(6),
    ArithConst(9),
    ArithConst(10),
    ArithConst(9),
    ArithConst(20),
    ArithConst(4),
    ArithConst(40),
  )

  Synthesize.synthesize(language, input, outputs)


}
