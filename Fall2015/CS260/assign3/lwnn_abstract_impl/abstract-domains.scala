package cs260.lwnn.abstracted.domains

import cs260.lwnn.syntax._
import cs260.lwnn.util._

import TypeAliases._

//——————————————————————————————————————————————————————————————————————————————
// ClassDefs
//
// the class definitions are invariant, so we can factor them out into
// one global version rather than having one per state as in the
// formal semantics

case object θ {
  type FieldMap = Map[Var, Type]
  type MethodMap = Map[MethodName, Method]

  var targetMap:Map[String, (FieldMap, MethodMap)] = Map[String, (FieldMap, MethodMap)]()

  def apply(className:String): (FieldMap, MethodMap) =
    targetMap(className)

  def +(new_val:(String, (FieldMap,MethodMap))) = {
    // Multiple definitions of classes not allowed
    assert(!(targetMap contains new_val._1))
    // Modify the targetMap by adding the provided key val pair
    targetMap = targetMap + new_val
    // Return the current object
    this
  }
}


//——————————————————————————————————————————————————————————————————————————————
// Locals

case class Locals( x2val:Map[Var, Value] ) {
  def apply( x:Var ): Value =
    x2val(x)

  def +( xv:(Var, Value) ): Locals = {
    // Note: you cannot and should not update self.
    assert(xv._1.name != "self")
    // All locals should be declared as parameters.
    assert(x2val contains xv._1)
    Locals( x2val + xv )
  }
}

//——————————————————————————————————————————————————————————————————————————————
// Heap
//
// NOTE: in this version, we always weakly update the heap. for
// convenience, a Heap maintains two maps, one for objects and one for
// continuation stacks. In other words, there will be a map for
// Address to Object and also a Map for Address to sets of
// continuation stacks. Thus, there will be two different methods for
// accessing the heap (one for accessing objects, one for accessing
// continuation stacks) and two different methods for updating the
// heap (ditto).

case class Heap(addr2obj:Map[Address, Object], addr2kont:Map[Address, Set[Seq[Kont]]] ) {
  def getObj( addr:Address ): Object =
    addr2obj(addr)

  def getKont( addr:Address ): Set[Seq[Kont]] =
    addr2kont(addr)

  def addObj( xv:(Address, Object) ): Heap = {
    Heap( addr2obj + xv, addr2kont )
  }

  def addKont( xv:(Address, Seq[Kont]) ): Heap = {
    val curr_ks: Option[Set[Seq[Kont]]] = addr2kont.get(xv._1)
    var updated_ks = Set(xv._2)
    if (curr_ks.isDefined) {
      updated_ks = curr_ks.get ++ updated_ks
    }
    Heap(addr2obj, addr2kont + ((xv._1, updated_ks)))
  }
}

//——————————————————————————————————————————————————————————————————————————————
// Value
//
// NOTE: the type system disallows many operations on disparate value
// types (including ⊔), but we need to define them in the
// implementation anyway or the compiler will complain. We'll just
// have them return a ⊥ value.

sealed abstract class Value {
  def is_⊥ : Boolean
  def ⊔( v:Value ): Value
  def +( v:Value ): Value
  def −( v:Value ): Value
  def ×( v:Value ): Value
  def ÷( v:Value ): Value
  def <( v:Value ): Value
  def ≤( v:Value ): Value
  def ∧( v:Value ): Value
  def ∨( v:Value ): Value
  def ≈( v:Value ): Value
  def ≠( v:Value ): Value
}

// we'll use the {+,0,−} abstract domain with the following lattice:
// 
//      ⊤
//     /|\
//    − 0 +
//     \|/
//      ⊥
//
sealed abstract class AInt {
  def +( v:AInt ): AInt = {
    v match {
      case BOT => BOT
      case _ => TOP
    }
  }

  def −( v:AInt ): AInt = {
    v match {
      case BOT => BOT
      case _ => TOP
    }
  }

  def ×( v:AInt ): AInt = {
    v match {
      case BOT => BOT
      case _ => TOP
    }
  }

  def ÷( v:AInt ): AInt = {
    v match {
      case BOT | ZERO => BOT
      case _ => TOP
    }
  }

  def <( v:AInt ): ABool = {
    v match {
      case BOT => Bool.⊥
      case _ => Bool.⊤
    }
  }

  def ≤( v:AInt ): ABool = {
    v match {
      case BOT => Bool.⊥
      case _ => Bool.⊤
    }
  }

  def ≈( v:AInt ): ABool = {
    v match {
      case BOT => Bool.⊥
      case _ => Bool.⊤
    }
  }

  def ≠( v:AInt ): ABool = {
    v match {
      case BOT => Bool.⊥
      case _ => Bool.⊤
    }
  }
}

//No need to do anything for TOP, default behavior is fine.
case object TOP extends AInt {
  override def toString = "TOP"
}

case object BOT extends AInt {
  override def +( v:AInt ): AInt = BOT

  override def −( v:AInt ): AInt = BOT

  override def ×( v:AInt ): AInt = BOT

  override def ÷( v:AInt ): AInt = BOT

  override def <( v:AInt ): ABool = Bool.⊥

  override def ≤( v:AInt ): ABool = Bool.⊥

  override def ≈( v:AInt ): ABool = Bool.⊥

  override def ≠( v:AInt ): ABool = Bool.⊥

  override def toString = "BOT"
}

case object NEG extends AInt {
  override def +( v:AInt ): AInt = v match {
    case POS => TOP
    case NEG => NEG
    case ZERO => NEG
    case TOP => TOP
    case BOT => BOT
  }

  override def −( v:AInt ): AInt = v match {
    case POS => NEG
    case NEG => TOP
    case ZERO => NEG
    case TOP => TOP
    case BOT => BOT
  }

  override def ×( v:AInt ): AInt = v match {
    case POS => NEG
    case NEG => POS
    case ZERO => ZERO
    case TOP => TOP
    case BOT => BOT
  }

  override def ÷( v:AInt ): AInt = v match {
    case POS => NEG
    case NEG => POS
    case ZERO | BOT => BOT
    case TOP => TOP
  }

  override def <( v:AInt ): ABool = v match {
    case POS | ZERO => TRUE
    case NEG | TOP => Bool.⊤
    case BOT => Bool.⊥
  }

  override def ≤( v:AInt ): ABool = v match {
    case POS | ZERO => TRUE
    case NEG | TOP => Bool.⊤
    case BOT => Bool.⊥
  }

  override def ≈( v:AInt ): ABool = v match {
    case POS | ZERO => FALSE
    case NEG | TOP => Bool.⊤
    case BOT => Bool.⊥
  }

  override def ≠( v:AInt ): ABool = v match {
    case POS | ZERO => TRUE
    case NEG | TOP => Bool.⊤
    case BOT => Bool.⊥
  }

  override def toString = "NEG"
}

object POS extends AInt {
  override def +( v:AInt ): AInt = v match {
    case POS | ZERO => POS
    case NEG | TOP => TOP
    case BOT => BOT
  }

  override def −( v:AInt ): AInt = v match {
    case POS | TOP => TOP
    case NEG | ZERO => POS
    case BOT => BOT
  }

  override def ×( v:AInt ): AInt = v match {
    case POS => POS
    case NEG => NEG
    case ZERO => ZERO
    case TOP => TOP
    case BOT => BOT
  }

  override def ÷( v:AInt ): AInt = v match {
    case POS => POS
    case NEG => NEG
    case ZERO | BOT => BOT
    case TOP => TOP
  }

  override def <( v:AInt ): ABool = v match {
    case ZERO | NEG => FALSE
    case POS | TOP => Bool.⊤
    case BOT => Bool.⊥
  }

  override def ≤( v:AInt ): ABool = v match {
    case ZERO | NEG => FALSE
    case POS | TOP => Bool.⊤
    case BOT => Bool.⊥
  }

  override def ≈( v:AInt ): ABool = v match {
    case NEG | ZERO => FALSE
    case POS | TOP => Bool.⊤
    case BOT => Bool.⊥
  }

  override def ≠( v:AInt ): ABool = v match {
    case NEG | ZERO => TRUE
    case POS | TOP => Bool.⊤
    case BOT => Bool.⊥
  }

  override def toString = "POS"
}

case object ZERO extends AInt {

  override def +( v:AInt ): AInt = v match {
    case POS | NEG | ZERO => v
    case _ => v + ZERO

  }

  override def −( v:AInt ): AInt = v match {
    case POS => NEG
    case NEG => POS
    case ZERO => ZERO
    case _ => v − ZERO
  }

  override def ×( v:AInt ): AInt = v match {
    case POS | NEG | ZERO => ZERO
    case _ => v × ZERO
  }

  override def ÷( v:AInt ): AInt = v match {
    case POS | NEG | TOP => ZERO
    case ZERO | BOT  => BOT
  }

  override def <( v:AInt ): ABool = v match {
    case POS => TRUE
    case NEG | ZERO => FALSE
    case TOP => Bool.⊤
    case BOT => Bool.⊥
  }

  override def ≤( v:AInt ): ABool = v match {
    case POS | ZERO => TRUE
    case NEG => FALSE
    case TOP => Bool.⊤
    case BOT => Bool.⊥
  }

 override def ≈( v:AInt ): ABool = v match {
   case ZERO => TRUE
   case POS | NEG => FALSE
   case TOP => Bool.⊤
   case BOT => Bool.⊥
 }

  override def ≠( v:AInt ): ABool = v match {
    case ZERO => FALSE
    case POS | NEG => TRUE
    case TOP => Bool.⊤
    case BOT => Bool.⊥
  }

  override def toString = "ZERO"
}

case class ℤ( vs: Set[AInt] ) extends Value {

  override def is_⊥ = {
    vs.size == 1 && vs.head == BOT
  }

  override def ⊔( v:Value ): Value = {
    v match {
      case z: ℤ => ℤ(vs ++ z.vs)
      case _ => ℤ(Set(ℤ.⊥))
    }
  }

  override def ∧( v:Value ): Value = {
    ℤ(Set(ℤ.⊥))
  }

  override def ∨( v:Value ): Value = {
    ℤ(Set(ℤ.⊥))
  }

  override def +( v:Value ) = v match {
    case z: ℤ => ℤ(for (x ← vs; y ← z.vs) yield x + y)
    case _ => ℤ(Set(ℤ.⊥))
  }

  override def −( v:Value ) = v match {
    case z: ℤ => ℤ(for ( x ← vs ; y ← z.vs ) yield x − y)
    case _ => ℤ(Set(ℤ.⊥))
  }

  override def ×( v:Value ) = v match {
    case z: ℤ => ℤ(for ( x ← vs ; y ← z.vs ) yield x × y)
    case _ => ℤ(Set(ℤ.⊥))
  }

  override def ÷( v:Value ) = v match {
    case z: ℤ => ℤ(for ( x ← vs ; y ← z.vs ) yield x ÷ y)
    case _ => ℤ(Set(ℤ.⊥))
  }

  override def <( v:Value ) = v match {
    case z: ℤ => Bool(for ( x ← vs ; y ← z.vs ) yield x < y)
    case _ => ℤ(Set(ℤ.⊥))
  }

  override def ≤( v:Value ) = v match {
    case z: ℤ => Bool(for ( x ← vs ; y ← z.vs ) yield x ≤ y)
    case _ => ℤ(Set(ℤ.⊥))
  }

  override def ≈( v:Value ) = v match {
    case z: ℤ => Bool(for ( x ← vs ; y ← z.vs ) yield x ≈ y)
    case _ => ℤ(Set(ℤ.⊥))
  }

  override def ≠( v:Value ) = v match {
    case z: ℤ => Bool(for ( x ← vs ; y ← z.vs ) yield x ≠ y)
    case _ => ℤ(Set(ℤ.⊥))
  }

  override def toString =
    "{ " + vs.mkString(":: ") + " }"
}

object ℤ {
  val ⊤ = TOP
  val ⊥ = BOT

  def α( ns:Set[BigInt] ): ℤ = {
    ℤ(ns map (n ⇒ if (n < 0) NEG else if (n == 0) ZERO else POS))
  }

  def α( n: BigInt ): ℤ = {
    α(Set(n))
  }
}

sealed abstract class ABool {


  def ∧(v:ABool): ABool = {
    v match {
      case BBOT => BBOT
      case _ => BTOP
    }
  }

  def ∨(v:ABool): ABool = {
    v match {
      case BBOT => BBOT
      case _ => BTOP
    }
  }

  def ≈(v:ABool): ABool = {
    v match {
      case BBOT => BBOT
      case _ => BTOP
    }
  }

  def ≠(v:ABool): ABool = {
    v match {
      case BBOT => BBOT
      case _ => BTOP
    }
  }

}

case object TRUE extends ABool {

  override def ∧(v:ABool): ABool = {
    v match {
      case TRUE => TRUE
      case FALSE => FALSE
      case _ => v
    }
  }

  override def ∨(v:ABool): ABool = {
    v match {
      case TRUE | FALSE => TRUE
      case _ => v
    }
  }

  override def ≈(v:ABool): ABool = {
    v match {
      case TRUE => TRUE
      case FALSE => FALSE
      case _ => v
    }
  }

  override def ≠(v:ABool): ABool = {
    v match {
      case TRUE => FALSE
      case FALSE => TRUE
      case _ => v
    }
  }

  override def toString = "True"
}

case object FALSE extends ABool {

  override def ∧(v:ABool): ABool = {
    v match {
      case TRUE | FALSE => FALSE
      case _ => v
    }
  }

  override def ∨(v:ABool): ABool = {
    v match {
      case TRUE => TRUE
      case FALSE => FALSE
      case _ => v
    }
  }

  override def ≈(v:ABool): ABool = {
    v match {
      case TRUE => FALSE
      case FALSE => TRUE
      case _ => v
    }
  }

  override def ≠(v:ABool): ABool = {
    v match {
      case TRUE => TRUE
      case FALSE => FALSE
      case _ => v
    }
  }

  override def toString = "False"
}

case object BBOT extends ABool {
  override def ∧(v:ABool): ABool = BBOT

  override def ∨(v:ABool): ABool = BBOT

  override def ≈(v:ABool): ABool = BBOT

  override def ≠(v:ABool): ABool = BBOT

  override def toString = "Bool_Bottom"
}

case object BTOP extends ABool {
  override def toString = "True, False"
}


// we'll use the (𝒫({true, false}), ⊆) abstract domain.
case class Bool( bs:Set[ABool] ) extends Value {
  override def is_⊥ = {
    bs.size == 1 && bs.head == BBOT
  }

  override def ⊔( v:Value ): Value = {
    v match {
      case b: Bool => Bool(bs ++ b.bs)
      case _ => Bool(Set(BBOT))
    }
  }
  override def +( v:Value ): Value = Bool(Set(BBOT))
  override def −( v:Value ): Value = Bool(Set(BBOT))
  override def ×( v:Value ): Value = Bool(Set(BBOT))
  override def ÷( v:Value ): Value = Bool(Set(BBOT))
  override def <( v:Value ): Value = Bool(Set(BBOT))
  override def ≤( v:Value ): Value = Bool(Set(BBOT))

  override def ∧( v:Value ): Value = {
    v match {
      case b: Bool => Bool( for ( x ← bs ; y ← b.bs) yield x ∧ y)
      case _ => sys.error("undefined behavior")
    }

  }
  override def ∨( v:Value ): Value = {
    v match {
      case b: Bool => Bool( for ( x ← bs ; y ← b.bs) yield x ∧ y)
      case _ => sys.error("undefined behavior")
    }
  }
  override def ≈( v:Value ): Value = {
    v match {
      case b: Bool => Bool( for ( x ← bs ; y ← b.bs) yield x ≈ y)
      case _ => Bool(Set(FALSE))
    }
  }
  override def ≠( v:Value ): Value = {
    v match {
      case b: Bool => Bool( for ( x ← bs ; y ← b.bs) yield x ≠ y)
      case _ => Bool(Set(TRUE))
    }
  }

  override def toString =
    "{ " + bs.mkString(":: ") + " }"
}

object Bool {
  val ⊤ = BTOP
  val ⊥ = BBOT

  def α( ns:Set[Boolean] ): Bool =
    Bool( ns map (n ⇒ if (n) TRUE else FALSE) )

  def α( n: Boolean ): Bool =
    α(Set(n))
}



sealed abstract class AStr {

  def +( v:AStr ): AStr

  def <( v:AStr ): ABool

  def ≤( v:AStr ): ABool

  def ≈( v:AStr ): ABool

  def ≠( v:AStr ): ABool
}

case object STOP extends AStr {
  override def +( v:AStr ): AStr = {
    v match {
      case SBOT => SBOT
      case _ => STOP
    }
  }

  override def <( v:AStr ): ABool = {
    v match {
      case SBOT => Bool.⊥
      case _ => Bool.⊤
    }
  }

  override def ≤( v:AStr ): ABool = {
    v match {
      case SBOT => Bool.⊥
      case _ => Bool.⊤
    }
  }

  override def ≈( v:AStr ): ABool = {
    v match {
      case SBOT => Bool.⊥
      case _ => Bool.⊤
    }
  }

  override def ≠( v:AStr ): ABool = {
    v match {
      case SBOT => Bool.⊥
      case _ => Bool.⊤
    }
  }

  override def toString = "ANY_STRING"
}

case object SBOT extends AStr {
  override def +( v:AStr ): AStr = SBOT

  override def <( v:AStr ): ABool = Bool.⊥

  override def ≤( v:AStr ): ABool = Bool.⊥

  override def ≈( v:AStr ): ABool = Bool.⊥

  override def ≠( v:AStr ): ABool = Bool.⊥

  override def toString = "NO_STRING"
}

// for strings we'll use the {⊥,⊤} domain s.t. ⊥ means no string and ⊤
// means any string, so the ordering is ⊥ ⊑ ⊤.
case class Str(ss: Set[AStr]) extends Value {

  override def is_⊥ = {
    ss.size == 1 && ss.head == SBOT
  }

  override def ⊔( v:Value ): Value = {
    v match {
      case s: Str => Str(s.ss ++ ss)
      case _ => Str(Set(SBOT))
    }
  }
  override def −( v:Value ): Value = Str(Set(SBOT))
  override def ×( v:Value ): Value = Str(Set(SBOT))
  override def ÷( v:Value ): Value = Str(Set(SBOT))
  override def ∧( v:Value ): Value = Str(Set(SBOT))
  override def ∨( v:Value ): Value = Str(Set(SBOT))

  override def +( v:Value ) = v match {
    case s: Str => Str(for (x ← ss; y ← s.ss) yield x + y)
    case _ => Str(Set(SBOT))
  }

  override def <( v:Value ) = v match {
    case s: Str => Bool(for ( x ← ss ; y ← s.ss ) yield x < y)
    case _ => Bool(Set(Bool.⊥))
  }

  override def ≤( v:Value ) = v match {
    case s: Str => Bool(for ( x ← ss ; y ← s.ss ) yield x ≤ y)
    case _ => Bool(Set(Bool.⊥))
  }

  override def ≈( v:Value ) = v match {
    case s: Str => Bool(for ( x ← ss ; y ← s.ss ) yield x ≈ y)
    case _ => Bool(Set(Bool.⊥))
  }

  override def ≠( v:Value ) = v match {
    case s: Str => Bool(for ( x ← ss ; y ← s.ss ) yield x ≠ y)
    case _ => Bool(Set(Bool.⊥))
  }

  override def toString =
    "{ " + ss.mkString(":: ") + " }"
}

object Str {
  val ⊤ = STOP
  val ⊥ = SBOT

  def α( strs:Set[String] ): Str =
    Str( strs map (n ⇒ if (n == None) SBOT else STOP) )

  def α( n: String ): Str =
    α(Set(n))
}

// for convenience we'll keep a set of addresses and separately a
// boolean indicating whether the reference could also be Null.
case class Reference( as:Set[Address], nil:Boolean = false ) extends Value {

  override def is_⊥ = {
    as.size == 0 && !nil
  }

  override def ⊔( v:Value ): Value = {
    v match {
      case r: Reference => Reference(as ++ r.as, nil||r.nil)
      case _ => Reference.⊥
    }
  }
  override def +( v:Value ): Value = Reference.⊥
  override def −( v:Value ): Value = Reference.⊥
  override def ×( v:Value ): Value = Reference.⊥
  override def ÷( v:Value ): Value = Reference.⊥
  override def <( v:Value ): Value = Bool(Set(Bool.⊥))
  override def ≤( v:Value ): Value = Bool(Set(Bool.⊥))
  override def ∧( v:Value ): Value = Reference.⊥
  override def ∨( v:Value ): Value = Reference.⊥
  override def ≈( v:Value ): Value = {
    v match {
      case r: Reference => Bool.α(r.as == as && r.nil == nil)
      case _ => Bool(Set(Bool.⊥))
    }
  }
  override def ≠( v:Value ): Value = {
    v match {
      case r: Reference => Bool.α(r.as != as && r.nil != nil)
      case _ => Bool(Set(Bool.⊥))
    }
  }


  override def toString =
    if ( as.isEmpty && nil ) "null"
    else if ( as.size == 1 && !nil ) as.head.toString
    else {
      val addrs = as map ( _.toString )
      val strs = if ( nil ) addrs + "null" else addrs
      "{" + strs.mkString(",") + "}"
    }
}

object Reference {
  val ⊥ = Reference(Set(), false)
  val Null = Reference(Set(), true)

  def apply( a:Address ): Reference =
    Reference(Set(a), false)
}

// abstract addresses will be the AST node id of the left-hand side
// variable of the New statement that allocates the address.
case class Address( loc:Int ) {
  override def toString =
    "addr" + loc
}

//——————————————————————————————————————————————————————————————————————————————
// Object

case class Object( cn:ClassName, flds:Map[Var, Value] ) {
  def ⊔( o:Object ): Object = {
    assert(o.cn == cn)
    val newflds = flds.foldRight(Map[Var, Value]())((currfld:(Var, Value), accu:Map[Var,Value]) => accu + ((currfld._1, currfld._2 ⊔ o.flds(currfld._1))))
    Object(cn, newflds)
  }

  def apply( x:Var ): Value =
    flds(x)

  def +( xv:(Var, Value) ): Object = {
    Object(cn, flds + xv)
  }
}

//——————————————————————————————————————————————————————————————————————————————
// Kont

sealed abstract class Kont
case class StmtK( s:Stmt ) extends Kont
case class WhileK( e:Exp, ss:Seq[Stmt] ) extends Kont
case class RetK( x:Var, e:Exp, ρ:Locals ) extends Kont
case class FinK( a:Address ) extends Kont
