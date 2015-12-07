package cs260.lwnn.abstracted.interpreter
import cs260.lwnn.syntax._
import cs260.lwnn.util._
import cs260.lwnn.abstracted.domains._
import cs260.lwnn.abstracted.helpers.Helpers
import cs260.lwnn.abstracted.helpers.Helpers.initstate

import scala.util._
import scala.collection.mutable.{ Set ⇒ MSet, Map ⇒ MMap }

import TypeAliases._

//——————————————————————————————————————————————————————————————————————————————
// Abstract interpreter entry point

object Abstract {

  def main( args:Array[String] ) {
    Parse.getAST( scala.io.Source.fromFile(args(0)).mkString ) match {
      // parsing error: program is not well-formed
      case Left(err) ⇒ println(err)

      // successfully parsed: program is well-formed
      case Right((classTable, ast:Program)) ⇒
        try {
          // throws Illtyped exception if program is not well-typed
          Typechecker.typecheck(ast, classTable)

          // program is well-formed and well-typed; ready to compute
          // fixpoint for collecting semantics
          //
          // NOTE: this version computes the MOP result, i.e., there
          //       is no widening.

          // worklist
          var work = Set[State]( initstate(ast) )

          // remember set
          val memo = MSet[State]()

          // compute fixpoint
          while ( work.nonEmpty ) {
            work = work.flatMap( _.next ).flatMap( ς ⇒
              if ( ς.fin )
                // if this is a final state, we don't need to do
                // anything
                None 
              else if ( ς.so.isEmpty && !ς.κs.head.isInstanceOf[FinK] )
                // we'll skip memoizing intermediate states (i.e.,
                // states with no statement) just to save space; go
                // ahead and put such states on the worklist
                Some(ς)
              else if ( !memo(ς) ) {
                // if the state does have a statement, and we have not
                // seen it before, memoize it and put it on the
                // worklist
                memo += ς
                Some(ς)
              }
              else
                // the state does have a statement, but we've seen it
                // before so we don't need to process it again
                None
            )
          }

          // output abstract values for Print statements: for each
          // Print, join all values for the printed expresion together
          // and output the result. Do this in ascending order of the
          // Print statements' node ids.
          val out = MMap[Int, Value]()
          memo foreach {
            case ς @ State(Some(print @ Print(e)), _, _, _) ⇒
              out get print.id match {
                case None ⇒
                  out(print.id) = ς.η(e)

                case Some(v) ⇒
                  out(print.id) = ς.η(e) ⊔ v
              }

            case _ ⇒ ()
          }
          out.toSeq.sortBy(_._1).foreach {
            case (id, v) ⇒ println(id + ": " + v)
          }
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

import cs260.lwnn.abstracted.domains.Heap
import cs260.lwnn.abstracted.domains.Kont
import cs260.lwnn.abstracted.domains.Locals
import cs260.lwnn.abstracted.domains.Address
import cs260.lwnn.abstracted.domains.Bool
import cs260.lwnn.abstracted.domains.Str
import cs260.lwnn.abstracted.domains.ℤ
import cs260.lwnn.syntax._

//——————————————————————————————————————————————————————————————————————————————
// State, transition rules, and η
//
// Note: Any undefined behavior of the program (i.e., that would
// result in a sys.error in the concrete semantics) should result in
// next returning an empty set of States in the abstract version. This
// includes if η returns a ⊥ value.

case class State( so:Option[Stmt], locls:Locals, heap:Heap, κs:Seq[Kont] ) {
  // is this a final state (i.e., the program has terminated)?
  def fin: Boolean =
    so.isEmpty && κs.isEmpty

  // we define η here so that we have access to ρ and σ without
  // needing to pass them in as parameters.
  def η( e:Exp ): Value =
    e match {

      case Nums(ns) =>
        ℤ.α(ns)

      case Bools(bs) =>
        Bool.α(bs)

      case Strs(sts) =>
        Str.α(sts)

      case Nulls() =>
        Reference.Null

      case v @ Var(x) =>
        locls(v)

      case Access(e,x) => {
        val a_val = η(e)
        //TODO: Handle NULL
        a_val match {
          case a:Reference => Helpers.lookup(a.as, x, heap)
          case _ => sys.error("Invalid Object to Access : undefined behavior")
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

      case _ => sys.error("Invalid Expression to Evaluate : Should never reach here.")
    }

  // the state transition relation.
  def next: Set[State] =
    so match {
      case Some(s) =>
        s match {
          // rule 1
          case Assign(x, e) =>
            η(e) match {
              case v:Value if (!v.is_⊥) => Set(copy(so=None, locls = locls + (x -> v)))
              case _ => Set()
            }

          // rule 2
          case Update(e1, x, e2) => {
            val a_val = η(e1)
            a_val match {
              // TODO: Need to handle Null
              case a:Reference if a.as.size > 0 => Set(copy(so=None, heap = Helpers.update(heap, a.as, x, η(e2))))
              case _ => Set()
            }
          }

          // rule 3
          case Call(x, e, mn, args) => {
            val a_val = η(e)
            a_val match {
              //TODO: Need to handle Null
              case a:Reference if a.as.size > 0 => {
                val args_val = args.map(e => η(e))
                val call_ret = Helpers.call(x, a.as, heap, mn, args_val, locls, κs)
                call_ret.map((curr_val:(Locals, Heap, Seq[Kont])) => copy(so=None, heap = curr_val._2, locls = curr_val._1, κs=curr_val._3))
              }
              case _ => Set()
            }
          }

          // rule 4
          case New(x, cn, args) => {
            val args_val = args.map(e => η(e))
            val cons_ret = Helpers.construct(x, cn, args_val, locls, heap, κs)
            Set(copy(so=None, heap = cons_ret._2, locls = cons_ret._1, κs=cons_ret._3))
          }

          // rule 5 and 6
          case If(e, ss1, ss2) =>
            η(e) match {
              case n:Bool if n.bs.contains(TRUE) || n.bs.contains(BTOP) => Set(copy(so=None, κs=Helpers.toSK(ss1) ++ κs))
              case n:Bool if n.bs.contains(FALSE) || n.bs.contains(BTOP) => Set(copy(so=None, κs=Helpers.toSK(ss2) ++ κs))
              case _ => Set()
            }
          // rules 7 and 8
          case While(e, ss) =>
            η(e) match {
              case n:Bool if n.bs.contains(TRUE) || n.bs.contains(BTOP) => Set(copy(so=None, κs=(Helpers.toSK(ss) :+ WhileK(e, ss)) ++ κs))
              case n:Bool if n.bs.contains(FALSE) || n.bs.contains(BTOP) => Set(copy(so=None))
              case _ => Set()
            }

          case Print(e) => {
            println(η(e))
            Set(copy(so=None))
          }

        }
      case None =>
        κs.head match {
          case FinK(a_k) => {
            heap.getKont(a_k).map(curr_ks => {
                curr_ks.head match {
                  case cret:RetK => copy(so = None, locls = cret.ρ + (cret.x -> η(cret.e) ), κs = curr_ks.tail)
                }
              }
            )
          }
          // rule 10
          case StmtK(s1) => Set(copy(so=Some(s1), κs = κs.tail))
          // rules 11 and 12
          case wk@WhileK(e, ss) =>
            η(e) match {
              case n:Bool if n.bs.contains(TRUE) || n.bs.contains(BTOP) => Set(copy(so=None, κs = Helpers.toSK(ss) ++ κs))
              case n:Bool if n.bs.contains(FALSE) || n.bs.contains(BTOP) => Set(copy(so=None, κs = κs.tail))
              case _ => Set()
            }
          case _ => Set()
        }
    }
}
