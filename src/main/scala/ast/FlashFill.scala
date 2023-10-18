import ast.{Expr, Language, Op, Type, Value, Var}

import java.io.File

/** Language */
object FlashFill extends Language[FlashFillExpr, FlashFillOps, FlashFillType, FlashFillValue] {

  override def parseIO(f: File): (List[List[FlashFillValue]], List[FlashFillValue]) = ???

  override def getOpInfo(op: FlashFillOps): (Int, List[FlashFillType], FlashFillType) =
    op match
      case AddOp | MinusOp => (2, List(IntegerType, IntegerType), IntegerType)
      case FindOp => (2, List(StringType, StringType), IntegerType)
      case LenOp => (1, List(StringType), IntegerType)
      case EqualsOp => (2, List(StringType, StringType), BooleanType)
      case GTOp | GTEOp => (2, List(IntegerType, IntegerType), BooleanType)
      case ConcatOp => (2, List(StringType, StringType), StringType)
      case LeftOp | RightOp => (2, List(StringType, IntegerType), StringType)
      case SubstrOp => (3, List(StringType, IntegerType, IntegerType), StringType)
      case ReplaceOp => (4, List(StringType, IntegerType, IntegerType, StringType), StringType)
      case TrimOp => (1, List(StringType), StringType)
      case RepeatOp => (2, List(StringType, IntegerType), StringType)
      case SubstituteOp => (3, List(StringType, StringType, StringType), StringType)
      case ToTextOp => (1, List(IntegerType), StringType)
      case LowercaseOp | UppercaseOp | PropercaseOp => (1, List(StringType), StringType)
      case IfOp => (3, List(BooleanType, StringType, StringType), StringType)

  override def ops: List[FlashFillOps] =
    List(
      AddOp,
      MinusOp,
      FindOp,
      LenOp,
      EqualsOp,
      GTOp,
      GTEOp,
      ConcatOp,
      LeftOp,
      RightOp,
      SubstrOp,
      ReplaceOp,
      TrimOp,
      RepeatOp,
      SubstituteOp,
      ToTextOp,
      LowercaseOp,
      UppercaseOp,
      PropercaseOp,
      IfOp,
    )

  implicit def narrowToStringExpr(e: FlashFillExpr): StringExpr = e.asInstanceOf[StringExpr]
  implicit def narrowToIntExpr(e: FlashFillExpr): IntegerExpr = e.asInstanceOf[IntegerExpr]
  implicit def narrowToBooleanExpr(e: FlashFillExpr): BooleanExpr = e.asInstanceOf[BooleanExpr]

  override def buildExpr(op: FlashFillOps, args: List[FlashFillExpr]): FlashFillExpr =
    op match
      case AddOp => IntegerSum(args(0), args(1))
      case MinusOp => IntegerMinus(args(0), args(1))
      case FindOp => Find(args(0), args(1))
      case LenOp => Len(args(0))
      case EqualsOp => Equals(args(0), args(1))
      case GTOp => GT(args(0), args(1))
      case GTEOp => GTE(args(0), args(1))
      case ConcatOp => Concat(args(0), args(1))
      case LeftOp => Left(args(0), args(1))
      case RightOp => Right(args(0), args(1))
      case SubstrOp => Substr(args(0), args(1), args(2))
      case ReplaceOp => Replace(args(0), args(1), args(2), args(3))
      case TrimOp => Trim(args(0))
      case RepeatOp => Repeat(args(0), args(1))
      case SubstituteOp => Substitute(args(0), args(1), args(2))
      case ToTextOp => ToText(args(0))
      case LowercaseOp => LowerCase(args(0))
      case UppercaseOp => UpperCase(args(0))
      case PropercaseOp => ProperCase(args(0))
      case IfOp => If(args(0), args(1), args(2))

  override def evalExpr(expr: FlashFillExpr): FlashFillValue =
    expr match
      case Equals(s1, s2) =>
        (evalExpr(s1), evalExpr(s2)) match
          case (res1: StringConst, res2: StringConst) => BooleanConst(res1.s == res2.s)
      case GT(i1, i2) =>
        (evalExpr(i1), evalExpr(i2)) match
          case (res1: IntConst, res2: IntConst) => BooleanConst(res1.i > res2.i)
      case GTE(i1, i2) =>
        (evalExpr(i1), evalExpr(i2)) match
          case (res1: IntConst, res2: IntConst) => BooleanConst(res1.i >= res2.i)
      case IntegerSum(i1, i2) =>
        (evalExpr(i1), evalExpr(i2)) match
          case (res1: IntConst, res2: IntConst) => IntConst(res1.i + res2.i)
      case IntegerMinus(i1, i2) =>
        (evalExpr(i1), evalExpr(i2)) match
          case (res1: IntConst, res2: IntConst) => IntConst(res1.i + res2.i)
      case Find(s1, s2) =>
        (evalExpr(s1), evalExpr(s2)) match
          case (res1: StringConst, res2: StringConst) => IntConst(res1.s.indexOf(res2.s))
      case Len(s) => IntConst(evalExpr(s).asInstanceOf[StringConst].s.length)
      case Concat(s1, s2) =>
        (evalExpr(s1), evalExpr(s2)) match
          case (res1: StringConst, res2: StringConst) => StringConst(res1.s.concat(res2.s))
      case Left(s, i) =>
        (evalExpr(s), evalExpr(i)) match
          case (res1: StringConst, res2: IntConst) => StringConst(res1.s.substring(0, res2.i))
      case Right(s, i) =>
        (evalExpr(s), evalExpr(i)) match
          case (res1: StringConst, res2: IntConst) => StringConst(res1.s.substring(res1.s.size - res2.i))
      case Substr(s, i1, i2) =>
        (evalExpr(s), evalExpr(i1), evalExpr(i2)) match
          case (res1: StringConst, res2: IntConst, res3: IntConst) => StringConst(res1.s.substring(res2.i, res3.i))
      case Replace(s1, i1, i2, s2) =>
        (evalExpr(s1), evalExpr(i1), evalExpr(i2), evalExpr(s2)) match
          case (res1: StringConst, res2: IntConst, res3: IntConst, res4: StringConst) => StringConst(res1.s.substring(0, res2.i) + res4 + res1.s.substring(res3.i))
      case Trim(s) =>
        StringConst(evalExpr(s).asInstanceOf[StringConst].s.trim)
      case Repeat(s, i) =>
        (evalExpr(s), evalExpr(i)) match
          case (res1: StringConst, res2: IntConst) => StringConst(res1.s.repeat(res2.i))
      case Substitute(s1, s2, s3) =>
        (evalExpr(s1), evalExpr(s2), evalExpr(s3)) match
          case (res1: StringConst, res2: StringConst, res3: StringConst) => StringConst(res1.s.replaceAll(res2.s, res3.s))
      case ToText(i) =>
        StringConst(evalExpr(i).asInstanceOf[IntConst].i.toString)
      case LowerCase(s) =>
        StringConst(evalExpr(s).asInstanceOf[StringConst].s.toLowerCase)
      case UpperCase(s) =>
        StringConst(evalExpr(s).asInstanceOf[StringConst].s.toUpperCase)
      case ProperCase(s) =>
        StringConst(evalExpr(s).asInstanceOf[StringConst].s.split(' ').map(_.capitalize).mkString(" "))
      case If(b, s1, s2) =>
        (evalExpr(b), evalExpr(s1), evalExpr(s2)) match
          case (res1: BooleanConst, res2: StringConst, res3: StringConst) => if (res1.b) res2 else res3
      case f: FlashFillValue => f
      case _ => throw RuntimeException("Unsupported Operation")

  override def extractConstants(inputs: List[List[FlashFillValue]], outputs: List[FlashFillValue]): Map[FlashFillExpr, List[FlashFillValue]] =
    var result: Map[FlashFillExpr, List[FlashFillValue]] = Map()
    for (input <- inputs) {
      for ((arg,idx) <- input.zipWithIndex) {
        val argvar = FlashFillVar("__input__" + idx)
        result += (argvar -> (result.getOrElse(argvar, List()) ++ List(arg)))
      }
    }
    def addConstant(ioSize: Int, vals: List[FlashFillValue]): Map[FlashFillExpr, List[FlashFillValue]] = {
      vals.map(f => (f, (for {x <- Range(0, ioSize)} yield f).toList)).toMap
    }

    result ++ addConstant(outputs.size, inputs.flatten ++ outputs ++ constants)


  val constants: List[FlashFillValue] = List(
    StringConst(" "),
    IntConst(0),
    IntConst(1),
    IntConst(2),
    IntConst(3),
  )
}


/** Types */
sealed trait FlashFillType extends Type
object IntegerType extends FlashFillType

object BooleanType extends FlashFillType

object StringType extends FlashFillType

/** Operations */
sealed trait FlashFillOps extends Op
case object AddOp extends FlashFillOps
case object MinusOp extends FlashFillOps
case object FindOp extends FlashFillOps
case object LenOp extends FlashFillOps
case object EqualsOp extends FlashFillOps
case object GTOp extends FlashFillOps
case object GTEOp extends FlashFillOps
case object ConcatOp extends FlashFillOps
case object LeftOp extends FlashFillOps
case object RightOp extends FlashFillOps
case object SubstrOp extends FlashFillOps
case object ReplaceOp extends FlashFillOps
case object TrimOp extends FlashFillOps
case object RepeatOp extends FlashFillOps
case object SubstituteOp extends FlashFillOps
case object ToTextOp extends FlashFillOps
case object LowercaseOp extends FlashFillOps
case object UppercaseOp extends FlashFillOps
case object PropercaseOp extends FlashFillOps
case object IfOp extends FlashFillOps

/** Expressions */
trait FlashFillExpr extends Expr {

}
trait FlashFillValue extends FlashFillExpr, Value {
}

sealed trait IntegerExpr extends FlashFillExpr {
  override def typ: Type = IntegerType


}

sealed trait BooleanExpr extends FlashFillExpr {
  override def typ: Type = BooleanType
}

sealed trait StringExpr extends FlashFillExpr {
  override def typ: Type = StringType
}


case class IntegerSum(i1: IntegerExpr, i2: IntegerExpr) extends IntegerExpr
case class IntegerMinus(i1: IntegerExpr, i2: IntegerExpr) extends IntegerExpr
case class Find(s1: StringExpr, s2: StringExpr) extends IntegerExpr
case class Len(s: StringExpr) extends IntegerExpr
case class Equals(s1: StringExpr, s2: StringExpr) extends BooleanExpr
case class GT(i1: IntegerExpr, i2: IntegerExpr) extends BooleanExpr
case class GTE(i1: IntegerExpr, i2: IntegerExpr) extends BooleanExpr
case class Concat(s1: StringExpr, s2: StringExpr) extends StringExpr
case class Left(s: StringExpr, i: IntegerExpr) extends StringExpr
case class Right(s: StringExpr, i: IntegerExpr) extends StringExpr
case class Substr(s: StringExpr, i1: IntegerExpr, i2: IntegerExpr) extends StringExpr
case class Replace(s1: StringExpr, i1: IntegerExpr, i2: IntegerExpr, s2: StringExpr) extends StringExpr
case class Trim(s: StringExpr) extends StringExpr
case class Repeat(s: StringExpr, i: IntegerExpr) extends StringExpr
case class Substitute(s1: StringExpr, s2: StringExpr, s3: StringExpr) extends StringExpr
case class ToText(i: IntegerExpr) extends StringExpr
case class LowerCase(s: StringExpr) extends StringExpr
case class UpperCase(s: StringExpr) extends StringExpr
case class ProperCase(s: StringExpr) extends StringExpr
case class If(b: BooleanExpr, s1: StringExpr, s2: StringExpr) extends StringExpr
case class IntConst(i: Int) extends IntegerExpr, FlashFillValue
case class StringConst(s: String) extends StringExpr, FlashFillValue
case class BooleanConst(b: Boolean) extends BooleanExpr, FlashFillValue
case class FlashFillVar(name: String) extends StringExpr, Var