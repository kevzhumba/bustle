import ast.{Expr, Language, Op, Type, Value, Var}

object Synthesize {

  /**
   * Recursively gets size combos for the number of args
   * @param target the target size
   * @param numArgs the number of args we need to get, including the current one
   * @return
   */
  def getSizeCombos(target: Int, numArgs: Int): List[List[Int]] =
    if (numArgs == 1) {
      List(List(target))
    } else {
      var result:List[List[Int]] = List()
      for (i <- Range(1, target + 2 - numArgs)) {
        val recurse = getSizeCombos(target - i, numArgs - 1)
        result = result ++ recurse.map(a => i :: a)
      }
      result
    }


  def getArgs[E <: Expr, O <: Op, T <: Type, V <: E](
                                                          w: Int,
                                                          e: Map[Int, Map[E, List[V]]],
                                                          numArgs: Int,
                                                          argTypes: List[T]
                                                        ): List[List[(E, List[V])]] =
    val possibleSizes = getSizeCombos(w-1, numArgs)
    var result: List[List[(E, List[V])]]= List()
    //For each of the arg sizes, we want to construct a list of all possible args give that list of sizes
    //How to do this? For each index in the arg size, gather all expressions that match. We need to get all combos right?
    for (argSizes <- possibleSizes) {
      var intermediate: List[List[(E, List[V])]] = List(List())
      for ((size, idx) <- argSizes.zipWithIndex) {
        val targetType = argTypes(idx)
        val validEntries = e(size).filter(kv => kv._1.typ == targetType).toList
        val crossProduct = for {x <- intermediate; y <- validEntries} yield y::x //this will lead everything to be in reverse order of arg index
        intermediate = crossProduct
      }
      result = intermediate.map(_.reverse) ++ result //so here we reverse each thing in the intermediate
    }
    result



  def synthesize[E <: Expr, O <: Op, T <: Type, V <: E](lang: Language[E, O, T, V], inputs: List[List[V]], outputs: List[V], n: Int = 10): E = {
    var e: Map[Int, Map[E, List[V]]] = Map()
    val constants = lang.extractConstants(inputs, outputs)
    var values: Set[List[V]] = constants.values.toSet
    e += (1 -> constants)
    for (w <- Range(2, n)) {
      println(w)
      for (op <- lang.ops) {
        val (numArgs, argTypes, retType) = lang.getOpInfo(op)
        if ((numArgs + 1) <= w) {
          val possibleArgs = getArgs(w, e, numArgs, argTypes)
          for (args <- possibleArgs) {
            if (args.nonEmpty) {
              val constructExprToAdd = lang.buildExpr(op, args.map(_._1))
              val constructExprsToEval = for {i <- Range(0, outputs.size)} yield lang.buildExpr(op, args.map(_._2(i))) // one for each io example
              //Exprs to eval are guaranteed to not have any variables
              try { //in case eval expr fails
                val results = constructExprsToEval.map(expr => lang.evalExpr(expr)).toList
                if (!values.contains(results)) {
                  values += results
                  e += (w -> (e.getOrElse(w, Map()) + (constructExprToAdd -> results)))
                }
                if (results == outputs) {
                  println(constructExprToAdd)
                  return constructExprToAdd
                }
              } catch
                case _ =>
            }
          }
        }
      }

    }
    throw RuntimeException("Unable to synthesize")
  }

}
