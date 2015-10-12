package cs260.lwnn.concrete.helpers

import cs260.lwnn.syntax._
import cs260.lwnn.util._
import cs260.lwnn.concrete.domains._
import cs260.lwnn.concrete.interpreter.State

import TypeAliases._

//——————————————————————————————————————————————————————————————————————————————
// Helper functions

object Helpers {

  def initclass(new_class:Class): (Map[Var,Type], Map[MethodName,Method]) = {
    val superflds =  θ(new_class.supercn)._1
    val supermethods = θ(new_class.supercn)._2
    val localflds = new_class.fields.foldRight(Map[Var,Type]())((accu:Map[Var,Type], toadd:Decl) => accu + ((toadd.x, toadd.τ)))
    val localmethods = new_class.methods.foldRight(Map[MethodName,Method]())((accu:Map[MethodName,Method], toadd:Method) => accu + ((toadd.mn, toadd)))
    val fields = superflds ++ localflds
    val methods = supermethods ++ localmethods
    (fields, methods)
  }

  def initstate( p:Program ): State = {
    val new_theta_map = p.classes.foldLeft("TopClass" -> (Map[Var, Type](), Map[MethodName, Method]()))((cl:Class, accu:Map) => accu + ((cl.cn, initclass(cl))))
    θ.targetMap = new_theta_map
    // Name of first class
    val cn = p.classes(0).cn
    //Fresh Address
    val a = Address()
    // fields of the first class
    val flds = θ(cn)._1.foldRight(Map[Var,Value]())((accu:Map[Var,Value], newfld:(Var, Type)) => accu + ((newfld._1, defaultvalue(newfld._2))))
    // new heap
    val newHeap = new Heap(Map((a, flds)))
    // get constructor
    val constructor = θ(cn)._2(cn)
    // new continuation stack
    val ks = toSk(constructor.body)
    // initilize locals
    val localvals = constructor.params.foldRight(Map[Var, Value]((new Var("self"), a)))((accu:Map[Var, Value], currd:Decl) => accu + ((currd.x, defaultvalue(currd.τ))))
    var newlocals = new Locals(localvals)
    // Return the initial state
    State(None, newlocals, newHeap, ks)
  }

  def toSK( ss:Seq[Stmt] ): Seq[Kont] =
    ss map ( StmtK(_) )
}
