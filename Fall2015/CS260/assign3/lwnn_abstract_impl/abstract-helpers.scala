package cs260.lwnn.abstracted.helpers

import cs260.lwnn.syntax._
import cs260.lwnn.util._
import cs260.lwnn.abstracted.domains._
import cs260.lwnn.abstracted.interpreter.State

import TypeAliases._

//——————————————————————————————————————————————————————————————————————————————
// Helper functions

object Helpers {
  // section 2.3.2; doesn't take θ because we've factored it out into a global.
  def call( x:Var, as:Set[Address], σ:Heap, mn:MethodName, vs:Seq[Value], ρ:Locals, κs:Seq[Kont] ): Set[(Locals, Heap, Seq[Kont])] =
    // ...

  // section 2.3.3; doesn't take θ because we've factored it out into a global.
  def construct( x:Var, cn:ClassName, vs:Seq[Value], ρ:Locals, σ:Heap, κs:Seq[Kont] ): (Locals, Heap, Seq[Kont]) =
    // ...

  // section 2.3.4
  def defaultvalue( τ:Type ): Value =  τ match {
    case IntT => ℤ.α(0)
    case BoolT => Bool.α(false)
    case StrT => Str.α("")
    case _ => Reference.Null
  }

  // section 2.3.5
  def initstate( p:Program ): State =
    // ...

  // section 2.3.6
  def lookup( as:Set[Address], x:Var, σ:Heap ): Value =
    // ...

  // section 2.3.7
  def toSK( ss:Seq[Stmt] ): Seq[Kont] =
    ss map ( StmtK(_) )

  // section 2.3.8
  def update( σ:Heap, as:Set[Address], x:Var, v:Value ): Heap = {
    if (as.size == 1 && σ.getObjs(as(0)).size == 1) {
      σ.addObj(as(0), Set(σ.getObjs(as(0)).head + (x, v)))
    } else {

    }
  }

  def update_addr(σ:Heap, curr_addr:Address, x:Var, v:Value): Heap = {
    σ.getObjs(curr_addr).foldRight(Set[Object]())((curr_obj:Object, accu:Set[Object]) => accu + (curr_obj ⊔ (curr_obj + ((x, v)))))
  }

}
