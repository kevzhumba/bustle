package ast

import java.io.File


object ArithmeticManipulation extends Language[ArithmeticManipulationExpr, ArithmeticManipulationOps, ArithmeticManipulationType, ArithmeticManipulationValue] {
  override def getOpInfo(op: ArithmeticManipulationOps): (Int, List[ArithmeticManipulationType], ArithmeticManipulationType) =
    op match
      case SumOp | MinusOp | MultOp | DivOp | ModOp => (2, List(IntegerType, IntegerType), IntegerType)
      case GTOp | GTEOp | EqOp => (2, List(IntegerType, IntegerType), BooleanType)
      case NegOp => (1, List(IntegerType), IntegerType)
      case NotOp => (1, List(BooleanType), BooleanType)
      case IfOp => (3, List(BooleanType, IntegerType, IntegerType), IntegerType)

  override def parseIO(f: File): (List[List[ArithmeticManipulationValue]], List[ArithmeticManipulationValue]) = ???

  override def evalExpr(expr: ArithmeticManipulationExpr): ArithmeticManipulationValue =
    expr match
      case Add(i1, i2) =>
        (evalExpr(i1), evalExpr(i2)) match
          case (ArithConst(res1), ArithConst(res2)) => ArithConst(res1 + res2)
      case Minus(i1, i2) =>
        (evalExpr(i1), evalExpr(i2)) match
          case (ArithConst(res1), ArithConst(res2)) => ArithConst(res1 - res2)
      case Div(i1, i2) =>
        (evalExpr(i1), evalExpr(i2)) match
          case (ArithConst(res1), ArithConst(res2)) => ArithConst(res1 / res2)
      case Mult(i1, i2) =>
        (evalExpr(i1), evalExpr(i2)) match
          case (ArithConst(res1), ArithConst(res2)) => ArithConst(res1 * res2)
      case Mod(i1, i2) =>
        (evalExpr(i1), evalExpr(i2)) match
          case (ArithConst(res1), ArithConst(res2)) => ArithConst(res1 % res2)
      case Neg(i1) =>
        evalExpr(i1) match
          case ArithConst(res1) => ArithConst(res1 * -1)
      case If(b1, i1, i2) =>
        (evalExpr(b1), evalExpr(i1), evalExpr(i2)) match
          case (BoolConst(cond), ArithConst(res1), ArithConst(res2)) => if (cond) ArithConst(res1) else ArithConst(res2)
      case ArithConst(i) => ArithConst(i)
      case Not(b) =>
        evalExpr(b) match
          case BoolConst(res1) => BoolConst(!res1)
      case GT(i1, i2) =>
        (evalExpr(i1), evalExpr(i2)) match
          case (ArithConst(res1), ArithConst(res2)) => BoolConst(res1 > res2)
      case GTE(i1, i2) =>
        (evalExpr(i1), evalExpr(i2)) match
          case (ArithConst(res1), ArithConst(res2)) => BoolConst(res1 >= res2)
      case Eq(i1, i2) =>
        (evalExpr(i1), evalExpr(i2)) match
          case (ArithConst(res1), ArithConst(res2)) => BoolConst(res1 == res2)
      case BoolConst(b) => BoolConst(b)


  override def ops: List[ArithmeticManipulationOps] =
    List(
      SumOp,
      MinusOp,
      MultOp,
      DivOp,
      ModOp,
      GTOp,
      NegOp,
      NotOp,
      GTEOp,
      EqOp,
      IfOp,
    )

  implicit def narrowToIntExpr(e: ArithmeticManipulationExpr): IntegerExpression = e.asInstanceOf[IntegerExpression]

  implicit def narrowToBooleanExpr(e: ArithmeticManipulationExpr): BooleanExpression = e.asInstanceOf[BooleanExpression]

  override def buildExpr(op: ArithmeticManipulationOps, args: List[ArithmeticManipulationExpr]): ArithmeticManipulationExpr =
    op match
      case SumOp => Add(args(0), args(1))
      case MinusOp => Minus(args(0), args(1))
      case MultOp => Mult(args(0), args(1))
      case DivOp => Div(args(0), args(1))
      case ModOp => Mod(args(0), args(1))
      case GTOp => GT(args(0), args(1))
      case NegOp => Neg(args(0))
      case NotOp => Not(args(0))
      case GTEOp => GTE(args(0), args(1))
      case EqOp => Eq(args(0), args(1))
      case IfOp => If(args(0), args(1), args(2))

  override def extractConstants(inputs: List[List[ArithmeticManipulationValue]], outputs: List[ArithmeticManipulationValue]): Map[ArithmeticManipulationExpr, List[ArithmeticManipulationValue]] =
    var result: Map[ArithmeticManipulationExpr, List[ArithmeticManipulationValue]] = Map()
    for (input <- inputs) {
      for ((arg, idx) <- input.zipWithIndex) {
        val argvar = ArithmeticManipulationVar("__input__" + idx)
        result += (argvar -> (result.getOrElse(argvar, List()) ++ List(arg)))
      }
    }
    def addConstant(ioSize: Int, vals: List[ArithmeticManipulationValue]): Map[ArithmeticManipulationExpr, List[ArithmeticManipulationValue]] = {
      vals.map(f => (f, (for {x <- Range(0, ioSize)} yield f).toList)).toMap
    }
    result ++ addConstant(outputs.size, inputs.flatten ++ outputs ++ constants)

  val constants: List[ArithmeticManipulationValue] =
    List(
      BoolConst(true),
      BoolConst(false),
    ) ++ Range(0, 101).toList.map(ArithConst(_))
}
/** Types */
sealed trait ArithmeticManipulationType extends Type {

}
case object IntegerType extends ArithmeticManipulationType
case object BooleanType extends ArithmeticManipulationType

/** Ops */
sealed trait ArithmeticManipulationOps extends Op
case object SumOp extends ArithmeticManipulationOps
case object MinusOp extends ArithmeticManipulationOps
case object MultOp extends ArithmeticManipulationOps
case object DivOp extends ArithmeticManipulationOps
case object ModOp extends ArithmeticManipulationOps
case object GTOp extends ArithmeticManipulationOps
case object NegOp extends ArithmeticManipulationOps
case object NotOp extends ArithmeticManipulationOps
case object GTEOp extends ArithmeticManipulationOps
case object EqOp extends ArithmeticManipulationOps
case object IfOp extends ArithmeticManipulationOps

/** Expressions */

trait ArithmeticManipulationExpr extends Expr {
}

sealed trait IntegerExpression extends ArithmeticManipulationExpr {
  override def typ: Type = IntegerType
}

sealed trait BooleanExpression extends ArithmeticManipulationExpr {
  override def typ: Type = BooleanType
}

case class Add(i1: IntegerExpression, i2: IntegerExpression) extends IntegerExpression
case class Minus(i1: IntegerExpression, i2: IntegerExpression) extends IntegerExpression
case class Mult(i1: IntegerExpression, i2: IntegerExpression) extends IntegerExpression
case class Div(i1: IntegerExpression, i2: IntegerExpression) extends IntegerExpression
case class Mod(i1: IntegerExpression, i2: IntegerExpression) extends IntegerExpression
case class Neg(i1: IntegerExpression) extends IntegerExpression
case class If(b: BooleanExpression, i1: IntegerExpression, i2: IntegerExpression) extends IntegerExpression
case class ArithConst(i: Int) extends IntegerExpression, ArithmeticManipulationValue


case class GT(i1: IntegerExpression, i2: IntegerExpression) extends BooleanExpression
case class GTE(i1: IntegerExpression, i2: IntegerExpression) extends BooleanExpression
case class Eq(i1: IntegerExpression, i2: IntegerExpression) extends BooleanExpression
case class Not(b: BooleanExpression) extends BooleanExpression
case class BoolConst(b: Boolean) extends BooleanExpression, ArithmeticManipulationValue

case class ArithmeticManipulationVar(name: String) extends ArithmeticManipulationExpr, IntegerExpression

trait ArithmeticManipulationValue extends ArithmeticManipulationExpr, Value { //element type denotes this values typ

}



