import ast.{ArithConst, ArithmeticManipulation, ArithmeticManipulationValue}

@main def main(args: String*): Unit = {
  if (args(0).toLowerCase == "string") {
    runStringTests()
  } else if (args(0).toLowerCase == "arith") {
    runArithTests()
  }
}
def runArithTests(): Unit = {
  val test1IO = (
    List(
      List(ArithConst(3), ArithConst(5)),
      List(ArithConst(2), ArithConst(6)),
      List(ArithConst(9), ArithConst(2)),
      List(ArithConst(10), ArithConst(8)),
      List(ArithConst(1), ArithConst(9)),
      List(ArithConst(20), ArithConst(20)),
      List(ArithConst(3), ArithConst(4)),
      List(ArithConst(25), ArithConst(40))
    ),
    List(
      ArithConst(5),
      ArithConst(6),
      ArithConst(9),
      ArithConst(10),
      ArithConst(9),
      ArithConst(20),
      ArithConst(4),
      ArithConst(40),
    )
  ) //max

  val test2IO = (
    List(
      List(ArithConst(3), ArithConst(5)),
      List(ArithConst(2), ArithConst(6)),
      List(ArithConst(9), ArithConst(2)),
      List(ArithConst(10), ArithConst(8)),
      List(ArithConst(1), ArithConst(9)),
      List(ArithConst(20), ArithConst(20)),
      List(ArithConst(3), ArithConst(4)),
      List(ArithConst(25), ArithConst(40))
    ),
    List(
      ArithConst(3),
      ArithConst(2),
      ArithConst(2),
      ArithConst(8),
      ArithConst(1),
      ArithConst(20),
      ArithConst(3),
      ArithConst(25),
    )
  ) //min

  val test3IO = (
    List(
      List(ArithConst(1)),
      List(ArithConst(2)),
      List(ArithConst(3)),
      List(ArithConst(4)),
      List(ArithConst(5)),
      List(ArithConst(6)),
    ),
    List(
      ArithConst(1),
      ArithConst(4),
      ArithConst(9),
      ArithConst(16),
      ArithConst(25),
      ArithConst(36),
    )
  )//x^2

  val test4IO = (
    List(
      List(ArithConst(1)),
      List(ArithConst(-1)),
      List(ArithConst(2)),
      List(ArithConst(-2)),
      List(ArithConst(212)),
      List(ArithConst(-132)),
    ),
    List(
      ArithConst(1),
      ArithConst(-1),
      ArithConst(1),
      ArithConst(-1),
      ArithConst(1),
      ArithConst(-1),
    )
  ) //sign

  val test5IO = (
    List(
      List(ArithConst(1)),
      List(ArithConst(-1)),
      List(ArithConst(2)),
      List(ArithConst(-2)),
      List(ArithConst(10)),
      List(ArithConst(-20)),
      List(ArithConst(56)),
      List(ArithConst(0)),
      List(ArithConst(-33))


    ),
    List(
      ArithConst(1),
      ArithConst(1),
      ArithConst(2),
      ArithConst(2),
      ArithConst(10),
      ArithConst(20),
      ArithConst(56),
      ArithConst(0),
      ArithConst(33),
    )
  )//abs
  val tests = List(
    test1IO,
    test2IO,
    test3IO,
    test4IO,
    test5IO,
  )
  tests.map((i,o) => Synthesize.synthesize(ArithmeticManipulation, i, o)).foreach(println(_))
}


def runStringTests(): Unit = {
  val test1IO = (
    List(
      List(StringConst("hello")),
      List(StringConst("world"))
    ),
    List(
      StringConst("h"),
      StringConst("w")
    )
  ) //Left(input(x), 1)

  val test2IO = (
    List(
      List(StringConst("hello")),
      List(StringConst("world"))
    ),
    List(
      StringConst("o"),
      StringConst("d")
    )
  ) //Right(input(x), 1)

  val test3IO = (
    List(
      List(StringConst("hello"), StringConst("you")),
      List(StringConst("world"), StringConst("domination"))
    ),
    List(
      StringConst("helloyou"),
      StringConst("worlddomination")
    )
  ) //Concatenate(input(x), input(y))

  val test4IO = (
    List(
      List(StringConst("hello"), StringConst("you")),
      List(StringConst("world"), StringConst("domination"))
    ),
    List(
      StringConst("hello you"),
      StringConst("world domination")
    )
  )//Concatenate(Concatenate(input(x), " "), input(y))

  val test5IO = (
    List(
      List(StringConst("hello")),
      List(StringConst("world")),
      List(StringConst("domination"))
    ),
    List(
      StringConst("ho"),
      StringConst("wd"),
      StringConst("dn")
    )
  )//Concatenate(Left(input(x), 1), Right(input(x), 1))

  val tests = List(test1IO, test2IO, test3IO, test4IO, test5IO)
  tests.map((i,o) => Synthesize.synthesize(FlashFill, i, o)).foreach(println(_))
}


