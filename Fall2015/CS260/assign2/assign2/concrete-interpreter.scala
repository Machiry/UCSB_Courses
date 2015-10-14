package cs260.lwnn.concrete.interpreter
import cs260.lwnn.syntax._
import cs260.lwnn.util._
import cs260.lwnn.concrete.domains._
import cs260.lwnn.concrete.helpers.Helpers._
import scala.util._

import TypeAliases._

//——————————————————————————————————————————————————————————————————————————————
// Concrete interpreter entry point

object Concrete {
  def main( args:Array[String] ) {
    // read program from file given as command-line argument and try to parse it
    Parse.getAST( scala.io.Source.fromFile(args(0)).mkString ) match {
      // parsing error: program is not well-formed
      case Left(err) ⇒ println(err)

      // successfully parsed: program is well-formed
      case Right((classTable, ast:Program)) ⇒
        try {
          // throws Illtyped exception if program is not well-typed
          Typechecker.typecheck(ast, classTable)

          // program is well-formed and well-typed; ready to interpret
          var curr_ς = initstate(ast)
          while ( !curr_ς.fin ) curr_ς = curr_ς.next
        }
        catch {
          // program is not well-typed
          case i:Illtyped ⇒ println(s"TypeError: ${i.msg}")
        }

      case _ ⇒
        sys.error("undefined behavior")
    }
  }
}



import cs260.lwnn.concrete.domains.{Kont, Locals}
import cs260.lwnn.concrete.helpers.Helpers
import cs260.lwnn.syntax.Stmt

//——————————————————————————————————————————————————————————————————————————————
// State, transition rules, and η
//
// Note: Any undefined behavior of the program (i.e., anything not
// explicitly covered in the formal semantics) should result in a
// system error like so: sys.error("undefined behavior")
// No need to pass theta, as its global and does not change between states
case class State( so:Option[Stmt], locls:Locals, heap:Heap, κs:Seq[Kont] ) {
  // is this a final state (i.e., the program has terminated)?
  def fin: Boolean =
    so.isEmpty && κs.isEmpty

  // we define η here so that we have access to ρ and σ without
  // needing to pass them in as parameters.
  def η( e:Exp ): Value =
    e match {

      case Nums(ns) =>
        ℤ( Random.shuffle(ns.toList).head )

      case Bools(bs) =>
        Bool(Random.shuffle(bs.toList).head)

      case Strs(sts) =>
        Str(Random.shuffle(sts.toList).head)

      case Nulls() =>
        Null

      case v @ Var(x) =>
        locls(v)

      case Access(e,x) => {
        val a_val = η(e)
        a_val match {
          case a:Address =>
            heap(a).var2value(x)
        }
      }

      case Binop(op, e1, e2) =>
        op match {
          case ⌜+⌝ => η(e1) + η(e2)
          case ⌜−⌝ => η(e1) − η(e2)
          case ⌜×⌝ => η(e1) × η(e2)
          case ⌜÷⌝ => η(e1) ÷ η(e2)
          case ⌜<⌝ => η(e1) < η(e2)
          case ⌜≤⌝ => η(e1) ≤ η(e2)
          case ⌜∧⌝ => η(e1) ∧ η(e2)
          case ⌜∨⌝ => η(e1) ∨ η(e2)
          case ⌜≈⌝ => η(e1) ≈ η(e2)
          case ⌜≠⌝ => η(e1) ≠ η(e2)
        }

    }


  // the state transition relation.
  def next: State =
    so match {
      case Some(s) =>
        s match {
          // rule 1
          case Assign(x, e) =>
            State(None, locls + (x -> η(e)), heap, κs)
          // rule 2
          case Update(e1, x, e2) => {
            val a_val = η(e1)
            a_val match {
              case a:Address => {
                val o = heap(a).update(x, η(e2))
                State(None, locls, heap + (a -> o), κs)
              }
            }
          }
          // rule 3
          case Call(x, e, mn, args) => {
            val a_val = η(e)
            a_val match {
              case a:Address => {
                val args_val = args.map(e => η(e))
                val call_ret = Helpers.call(x, a, heap, mn, args_val, locls)
                State(None, call_ret._1, heap, call_ret._2 ++ κs)
              }
            }
          }
          // rule 4
          case New(x, cn, args) => {
            val args_val = args.map(e => η(e))
            val cons_ret = Helpers.constructor(x, cn, args_val, locls, heap)
            State(None, cons_ret._1, cons_ret._2, cons_ret._3 ++ κs)
          }
          // rule 5 and 6
          case If(e, ss1, ss2) =>
            η(e) match {
              case Bool(n) if n => State(None, locls, heap, Helpers.toSK(ss1) ++ κs)
              case _ => State(None, locls, heap, Helpers.toSK(ss2) ++ κs)
            }
          // rules 7 and 8
          case While(e, ss) =>
            η(e) match {
              case Bool(n) if n =>
                State(None, locls, heap, (Helpers.toSK(ss) :+ WhileK(e, ss)) ++ κs)
              case _ => State(None, locls, heap, κs)
            }
          // Implement printing: copied from IMP implementation.
          case Print(e) => {
            println(s.id + ":" + η(e))
            State(None, locls, heap, κs)
          }
        }

      case None =>
        κs.head match {
          // rule 9
          case RetK(x, e, locs) =>
            State(None, locs + (x -> η(e)), heap, κs.tail)
          // rule 10
          case StmtK(s1) =>
            State(Some(s1), locls, heap, κs.tail)

          // rules 11 and 12
          case wk@WhileK(e, ss) =>
            η(e) match {
              case Bool(n) if n => State(None, locls, heap, (toSK(ss) :+ wk) ++ κs.tail)
              case _ => State(None, locls, heap, κs.tail)
            }
        }
    }
}
