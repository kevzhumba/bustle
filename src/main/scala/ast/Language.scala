package ast

import java.io.File

trait Language[E <: Expr, O <: Op, T <: Type, V <: E] {
  def ops: List[O]

  def getOpInfo(op: O): (Int, List[T], T)

  def parseIO(f: File): (List[List[V]], List[V])

  def evalExpr(expr: E): V

  def buildExpr(op: O, args: List[E]): E

  def extractConstants(inputs: List[List[V]], outputs: List[V]): Map[E, List[V]]

}
