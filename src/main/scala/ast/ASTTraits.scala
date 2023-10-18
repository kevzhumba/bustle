package ast

trait Op
trait Type
trait Expr {
  def typ: Type
}
trait Value extends Expr
