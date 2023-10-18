

@main def main(args: String*): Unit = {
  val language = FlashFill
  val input: List[List[FlashFillValue]] = List(
    List(
      StringConst("hello")
    ),
    List(StringConst("world")),
    List(StringConst("domination")),
    )


  val outputs: List[FlashFillValue] = List(
    StringConst("ho"),
    StringConst("wd"),
    StringConst("dn")
  )

  Synthesize.synthesize(language, input, outputs)


}
