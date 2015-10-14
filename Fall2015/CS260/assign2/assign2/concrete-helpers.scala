package cs260.lwnn.concrete.helpers

import cs260.lwnn.concrete.domains.θ.MethodMap
import cs260.lwnn.concrete.domains.θ.FieldMap
import cs260.lwnn.syntax._
import cs260.lwnn.util._
import cs260.lwnn.concrete.domains._
import cs260.lwnn.concrete.interpreter.State

import TypeAliases._

//——————————————————————————————————————————————————————————————————————————————
// Helper functions

object Helpers {

  def toSK( ss:Seq[Stmt] ): Seq[Kont] =
    ss map ( StmtK(_) )

  def initclass(class_map:Map[String, (FieldMap, MethodMap)], new_class:Class): (Map[Var,Type], Map[MethodName,Method]) = {
    val superflds =  class_map(new_class.supercn)._1
    val supermethods = class_map(new_class.supercn)._2
    val localflds = new_class.fields.foldRight(Map[Var,Type]())((toadd:Decl, accu:Map[Var,Type]) => accu + ((toadd.x, toadd.τ)))
    val localmethods = new_class.methods.foldRight(Map[MethodName,Method]())((toadd:Method, accu:Map[MethodName,Method]) => accu + ((toadd.mn, toadd)))
    val fields = superflds ++ localflds
    val methods = supermethods ++ localmethods
    (fields, methods)
  }

  def initstate( p:Program ): State = {
    val new_theta_map = p.classes.foldLeft(Map("TopClass" -> (Map[Var, Type](), Map[MethodName, Method]())))((accu:Map[ClassName, (Map[Var, Type], Map[MethodName, Method])], cl:Class) => accu + ((cl.cn, initclass(accu, cl))))
    θ.targetMap = new_theta_map
    // Name of first class
    val cn = p.classes(0).cn
    //Fresh Address
    val a = Address()
    // fields of the first class
    val flds = θ(cn)._1.foldRight(Map[Var,Value]())((newfld:(Var, Type), accu:Map[Var,Value]) => accu + ((newfld._1, defaultvalue(newfld._2))))
    // new heap
    val newHeap = Heap(Map((a, Object(cn, flds))))
    // get constructor
    val constructor = θ(cn)._2(cn)
    // new continuation stack
    val ks = toSK(constructor.body)
    // initilize locals
    val localvals = constructor.params.foldRight(Map[Var, Value]())((currd:Decl, accu:Map[Var, Value]) => accu + ((currd.x, defaultvalue(currd.τ))))

    val newlocals = Locals(localvals + ((constructor.params(0).x, a)))
    // Return the initial state
    State(None, newlocals, newHeap, ks)
  }

  def call(x:Var, a:Address, curr_heap:Heap, mn:MethodName, v:Seq[Value], curr_locals:Locals): (Locals, Seq[Kont]) = {
    // Get class name
    val cn = curr_heap(a).className
    val methods =  θ(cn)._2
    // Get current method
    val curr_method = methods(mn)
    // Create new continuation stack
    val new_ks = toSK(curr_method.body) :+ RetK(x, curr_method.rete, curr_locals)

    // first update everything with default value
    var new_locals = curr_method.params.splitAt(1)._1.foldRight(Map[Var, Value]())((currd:Decl, accu:Map[Var, Value]) => accu + ((currd.x, defaultvalue(currd.τ))))
    // Add self i.e first parameter
    new_locals = new_locals + ((curr_method.params(0).x, a))
    // next, zip params and v and replace the default value with passed value
    new_locals = (curr_method.params.splitAt(1)._2 zip v).foldRight(new_locals)((par_val:(Decl,Value), accu:Map[Var, Value]) => accu + ((par_val._1.x, par_val._2)))

    (Locals(new_locals), new_ks)
  }

  def constructor(x:Var, mn:ClassName, v:Seq[Value], curr_locals:Locals, curr_heap:Heap): (Locals, Heap, Seq[Kont]) = {
    // Fresh address
    val a = Address()
    // get all fields
    val flds = θ(mn)._1.foldRight(Map[Var,Value]())((newfld:(Var, Type), accu:Map[Var,Value]) => accu + ((newfld._1, defaultvalue(newfld._2))))
    // new object
    val o = Object(mn, flds)
    // insert the created object into heap
    val new_heap = curr_heap + ((a, o))

    val methods = θ(mn)._2

    val constructor_meth = methods(mn)

    // New continuation stack
    val new_ks = toSK(constructor_meth.body) :+ RetK(x, constructor_meth.rete, curr_locals)

    // first update everything with default value
    var new_locals = constructor_meth.params.splitAt(1)._1.foldRight(Map[Var, Value]())((currd:Decl, accu:Map[Var, Value]) => accu + ((currd.x, defaultvalue(currd.τ))))
    // Add self i.e first parameter
    new_locals = new_locals + ((constructor_meth.params(0).x, a))
    // next, zip params and v and replace the default value with passed value
    new_locals = (constructor_meth.params.splitAt(1)._2 zip v).foldRight(new_locals)((par_val:(Decl,Value), accu:Map[Var, Value]) => accu + ((par_val._1.x, par_val._2)))

    (Locals(new_locals), new_heap, new_ks)
  }
}
