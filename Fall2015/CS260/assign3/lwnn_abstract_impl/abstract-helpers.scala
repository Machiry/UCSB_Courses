package cs260.lwnn.abstracted.helpers

import cs260.lwnn.abstracted.domains.θ.MethodMap
import cs260.lwnn.abstracted.domains.θ.FieldMap
import cs260.lwnn.abstracted.domains.Value
import cs260.lwnn.abstracted.domains.θ
import cs260.lwnn.abstracted.domains.Heap
import cs260.lwnn.abstracted.domains.Object
import cs260.lwnn.abstracted.domains.Locals
import cs260.lwnn.abstracted.domains.Value
import cs260.lwnn.abstracted.interpreter.State
import cs260.lwnn.abstracted.domains.RetK
import cs260.lwnn.syntax._
import cs260.lwnn.util._
import cs260.lwnn.abstracted.domains._
import TypeAliases._

//——————————————————————————————————————————————————————————————————————————————
// Helper functions

object Helpers {

  def call_single_addr(x:Var, as:Address, σ:Heap, m:Method, vs:Seq[Value], ρ:Locals, κs:Seq[Kont] ): (Locals, Heap, Seq[Kont]) = {
    val a_k = Address(m.id)

    var new_locals = m.params.tail.foldRight(Map[Var, Value]())((currd:Decl, accu:Map[Var, Value]) => accu + ((currd.x, defaultvalue(currd.τ))))
    // Add self i.e first parameter
    new_locals = new_locals + ((m.params.head.x, Reference(as)))
    // next, zip params and v and replace the default value with passed value
    new_locals = (m.params.tail zip vs).foldRight(new_locals)((par_val:(Decl,Value), accu:Map[Var, Value]) => accu + ((par_val._1.x, par_val._2)))

    val a_k_target = Seq(RetK(x, m.rete, ρ)) ++ κs

    val new_heap = σ.addKont((a_k, a_k_target))

    val new_ks = toSK(m.body) :+ FinK(a_k)

    (Locals(new_locals), new_heap, new_ks)

  }
  // section 2.3.2; doesn't take θ because we've factored it out into a global.
  def call( x:Var, as:Set[Address], σ:Heap, mn:MethodName, vs:Seq[Value], ρ:Locals, κs:Seq[Kont] ): Set[(Locals, Heap, Seq[Kont])] = {
    val a_m = as.foldRight(Set[(Address, Method)]())((curra:Address, accu:Set[(Address, Method)]) => accu + ((curra, θ(σ.getObj(curra).cn)._2(mn))))
    a_m.foldRight(Set[(Locals, Heap, Seq[Kont])]())((curr_am:(Address, Method), accu:Set[(Locals, Heap, Seq[Kont])]) => accu + call_single_addr(x, curr_am._1, σ, curr_am._2, vs, ρ, κs))
  }

  // section 2.3.3; doesn't take θ because we've factored it out into a global.
  def construct( x:Var, cn:ClassName, vs:Seq[Value], ρ:Locals, σ:Heap, κs:Seq[Kont] ): (Locals, Heap, Seq[Kont]) = {
    val a = Address(x.id)
    // Get methods for the current class
    val methods = θ(cn)._2
    // Get the constructor method
    val constructor_meth = methods(cn)
    // Address for the continuation stack
    val a_k = Address(constructor_meth.id)

    val flds = θ(cn)._1.foldRight(Map[Var,Value]())((newfld:(Var, Type), accu:Map[Var,Value]) => accu + ((newfld._1, defaultvalue(newfld._2))))
    // new object
    val o = Object(cn, flds)

    // first update everything with default value
    var new_locals = constructor_meth.params.tail.foldRight(Map[Var, Value]())((currd:Decl, accu:Map[Var, Value]) => accu + ((currd.x, defaultvalue(currd.τ))))
    // Add self i.e first parameter
    new_locals = new_locals + ((constructor_meth.params.head.x, Reference(a)))
    // next, zip params and v and replace the default value with passed value
    new_locals = (constructor_meth.params.tail zip vs).foldRight(new_locals)((par_val:(Decl,Value), accu:Map[Var, Value]) => accu + ((par_val._1.x, par_val._2)))

    val a_k_target = Seq(RetK(x, constructor_meth.rete, ρ)) ++ κs

    var target_object = o

    if (σ.addr2obj.contains(a)) {
      target_object = σ.getObj(a) ⊔ o
    }

    var new_heap = σ.addObj((a, target_object))
    new_heap = new_heap.addKont((a_k, a_k_target))

    // new continuation stack
    val ks = toSK(constructor_meth.body) :+ FinK(a_k)

    (Locals(new_locals), new_heap, ks)

  }

  // section 2.3.4
  def defaultvalue( τ:Type ): Value =  τ match {
    case IntT => ℤ.α(0)
    case BoolT => Bool.α(false)
    case StrT => Str.α("")
    case _ => Reference.Null
  }

  // Section 2.3.5 initclass
  def initclass(class_map:Map[String, (FieldMap, MethodMap)], new_class:Class): (Map[Var,Type], Map[MethodName,Method]) = {
    val superflds =  class_map(new_class.supercn)._1
    val supermethods = class_map(new_class.supercn)._2
    val localflds = new_class.fields.foldRight(Map[Var,Type]())((toadd:Decl, accu:Map[Var,Type]) => accu + ((toadd.x, toadd.τ)))
    val localmethods = new_class.methods.foldRight(Map[MethodName,Method]())((toadd:Method, accu:Map[MethodName,Method]) => accu + ((toadd.mn, toadd)))
    val fields = superflds ++ localflds
    val methods = supermethods ++ localmethods
    (fields, methods)
  }

  // section 2.3.5
  def initstate( p:Program ): State = {
    val new_theta_map = p.classes.foldLeft(Map("TopClass" -> (Map[Var, Type](), Map[MethodName, Method]())))((accu:Map[ClassName, (Map[Var, Type], Map[MethodName, Method])], cl:Class) => accu + ((cl.cn, initclass(accu, cl))))
    θ.targetMap = new_theta_map

    val cn = p.classes(0).cn
    val fresh_addr = Address(p.classes(0).id)

    // fields of the first class
    val flds = θ(cn)._1.foldRight(Map[Var,Value]())((newfld:(Var, Type), accu:Map[Var,Value]) => accu + ((newfld._1, defaultvalue(newfld._2))))
    // new heap
    val newHeap = Heap(Map((fresh_addr, Object(cn, flds))), Map[Address, Set[Seq[Kont]]]())
    // get constructor
    val constructor = θ(cn)._2(cn)
    // new continuation stack
    val ks = toSK(constructor.body)
    // initialize locals
    val localvals = constructor.params.tail.foldRight(Map[Var, Value]())((currd:Decl, accu:Map[Var, Value]) => accu + ((currd.x, defaultvalue(currd.τ))))
    // Add self
    val newlocals = Locals(localvals + ((constructor.params.head.x, Reference(fresh_addr))))

    // Return the initial state
    State(None, newlocals, newHeap, ks)
  }


  // section 2.3.6
  def lookup( as:Set[Address], x:Var, σ:Heap ): Value = {
    assert(as.size > 0)
    val curr_var = σ.getObj(as.head)(x)
    as.tail.foldRight(curr_var)((curra:Address, accu:Value) => accu ⊔ σ.getObj(curra)(x))
  }

  // section 2.3.7
  def toSK( ss:Seq[Stmt] ): Seq[Kont] =
    ss map ( StmtK(_) )

  // section 2.3.8
  def update( σ:Heap, as:Set[Address], x:Var, v:Value ): Heap = {
    assert(as.size > 0)
    as.foldRight(σ)((curra:Address, accu:Heap) => accu.addObj(curra, σ.getObj(curra) ⊔ (σ.getObj(curra) + ((x, v)))))
  }

}
