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

  // ... (same as for the concrete semantics)
}


//——————————————————————————————————————————————————————————————————————————————
// Locals

case class Locals( /* ... */ ) {
  // ...
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

case class Heap( /* ... */ ) {
  // ...
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
  def ⊔( v:Value ): Value = sys.error("undefined behavior")
  def +( v:Value ): Value = sys.error("undefined behavior")
  def −( v:Value ): Value = sys.error("undefined behavior")
  def ×( v:Value ): Value = sys.error("undefined behavior")
  def ÷( v:Value ): Value = sys.error("undefined behavior")
  def <( v:Value ): Value = sys.error("undefined behavior")
  def ≤( v:Value ): Value = sys.error("undefined behavior")
  def ∧( v:Value ): Value = sys.error("undefined behavior")
  def ∨( v:Value ): Value = sys.error("undefined behavior")
  def ≈( v:Value ): Value = sys.error("undefined behavior")
  def ≠( v:Value ): Value = sys.error("undefined behavior")
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
  def +( v:AInt ): AInt = TOP

  def −( v:AInt ): AInt = TOP

  def ×( v:AInt ): AInt = TOP

  def ÷( v:AInt ): AInt = TOP

  def <( v:AInt ): Set[Boolean] = Bool.⊤

  def ≤( v:AInt ): Set[Boolean] = Bool.⊤

  def ≈( v:AInt ): Set[Boolean] = v match {
    case ZERO => v ≈ this
    case _ => Bool.⊤
  }

  def ≠( v:AInt ): Set[Boolean] = v match {
    case ZERO => v ≠ this
    case _ => Bool.⊤
  }
}

//No need to do anything for TOP, default behavior is fine.
case object TOP extends AInt

case object BOT extends AInt {
  override def +( v:AInt ): AInt = BOT

  override def −( v:AInt ): AInt = BOT

  override def ×( v:AInt ): AInt = BOT

  override def ÷( v:AInt ): AInt = BOT

  override def <( v:AInt ): Set[Boolean] = Bool.⊥

  override def ≤( v:AInt ): Set[Boolean] = Bool.⊥

  override def ≈( v:AInt ): Set[Boolean] = Bool.⊥

  override def ≠( v:AInt ): Set[Boolean] = Bool.⊥
}

case object NEG extends AInt {
  override def +( v:AInt ): AInt = v match {
    case POS => TOP
    case NEG => NEG
    case _ => v + NEG
  }

  override def −( v:AInt ): AInt = v match {
    case POS => NEG
    case NEG => TOP
    case _ => v − NEG
  }

  override def ×( v:AInt ): AInt = v match {
    case POS => NEG
    case NEG => POS
    case _ => v × NEG
  }

  override def ÷( v:AInt ): AInt = v match {
    case POS => NEG
    case NEG => POS
    case _ => v ÷ NEG
  }

  override def <( v:AInt ): Set[Boolean] = v match {
    case POS => Set(true)
    case NEG => Bool.⊤
    case _ => v < NEG
  }

  override def ≤( v:AInt ): Set[Boolean] = v match {
    case POS => Set(true)
    case NEG => Bool.⊤
    case _ => v ≤ NEG
  }

  override def ≈( v:AInt ): Set[Boolean] = v match {
    case POS => Set(false)
    case NEG => Bool.⊤
    case _ => v ≈ NEG
  }

  override def ≠( v:AInt ): Set[Boolean] = v match {
    case POS => Set(true)
    case NEG => Bool.⊤
    case _ => v ≠ NEG
  }
}

object POS extends AInt {
  override def +( v:AInt ): AInt = v match {
    case POS => POS
    case NEG => TOP
    case _ => v + POS
  }

  override def −( v:AInt ): AInt = v match {
    case POS => TOP
    case NEG => POS
    case _ => v − POS
  }

  override def ×( v:AInt ): AInt = v match {
    case POS => POS
    case NEG => NEG
    case _ => v × POS
  }

  override def ÷( v:AInt ): AInt = v match {
    case POS => POS
    case NEG => NEG
    case _ => v ÷ POS
  }

  override def <( v:AInt ): Set[Boolean] = v match {
    case POS => Bool.⊤
    case NEG => Set(false)
    case _ => v < POS
  }

  override def ≤( v:AInt ): Set[Boolean] = v match {
    case POS => Bool.⊤
    case NEG => Set(false)
    case _ => v ≤ POS
  }

  override def ≈( v:AInt ): Set[Boolean] = v match {
    case NEG => Set(false)
    case POS => Bool.⊤
    case _ => v ≈ POS
  }

  override def ≠( v:AInt ): Set[Boolean] = v match {
    case NEG => Set(true)
    case POS => Bool.⊤
    case _ => v ≠ POS
  }
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
    case POS | NEG => ZERO
    case ZERO => TOP
    case _ => v ÷ ZERO
  }

  override def <( v:AInt ): Set[Boolean] = v match {
    case POS => Set(true)
    case NEG | ZERO => Set(false)
    case _ => v < ZERO
  }

  override def ≤( v:AInt ): Set[Boolean] = v match {
    case POS | ZERO => Set(true)
    case NEG => Set(false)
    case _ => v ≤ ZERO
  }

 override def ≈( v:AInt ): Set[Boolean] = v match {
   case ZERO => Set(true)
   case POS | NEG => Set(false)
   case _ => v ≈ ZERO
 }

  override def ≠( v:AInt ): Set[Boolean] = v match {
    case ZERO => Set(false)
    case POS | NEG => Set(true)
    case _ => v ≈ ZERO
  }
}

case class ℤ( vs: Set[AInt] ) extends Value {
  override def +( v:Value ) = v match {
    case z: ℤ => ℤ(for (x ← vs; y ← z.vs) yield x + y)
    case _ => sys.error("undefined behavior: Cannot add non-Z to Z")
  }

  override def −( v:Value ) = v match {
    case z: ℤ => ℤ(for ( x ← vs ; y ← z.vs ) yield x − y)
    case _ => sys.error("undefined behavior: Cannot subtract non-Z from Z")
  }

  override def ×( v:Value ) = v match {
    case z: ℤ => ℤ(for ( x ← vs ; y ← z.vs ) yield x × y)
    case _ => sys.error("undefined behavior: Cannot multiply non-Z into Z")
  }

  override def ÷( v:Value ) = v match {
    case z: ℤ => ℤ(for ( x ← vs ; y ← z.vs ) yield x ÷ y)
    case _ => sys.error("undefined behavior: Cannot divide Z by non-Z")
  }

  override def <( v:Value ) = v match {
    case z: ℤ => Bool((for ( x ← vs ; y ← z.vs ) yield x < y).flatten)
    case _ => sys.error("undefined behavior: Cannot compare Z by non-Z")
  }

  override def ≤( v:Value ) = v match {
    case z: ℤ => Bool((for ( x ← vs ; y ← z.vs ) yield x ≤ y).flatten)
    case _ => sys.error("undefined behavior: Cannot compare Z by non-Z")
  }

  override def ≈( v:Value ) = v match {
    case z: ℤ => Bool((for ( x ← vs ; y ← z.vs ) yield x ≈ y).flatten)
    case _ => sys.error("undefined behavior: Cannot compare Z by non-Z")
  }

  override def ≠( v:Value ) = v match {
    case z: ℤ => Bool((for ( x ← vs ; y ← z.vs ) yield x ≠ y).flatten)
    case _ => sys.error("undefined behavior: Cannot compare Z by non-Z")
  }

  override def toString =
    "{ " + vs.mkString(", ") + " }"
}

object ℤ {
  val ⊤ = TOP
  val ⊥ = BOT

  def α( ns:Set[BigInt] ): ℤ =
    ℤ( ns map (n ⇒ if (n < 0) NEG else if (n == 0) ZERO else POS) )

  def α( n: BigInt ): ℤ =
    α(Set(n))
}

// we'll use the (𝒫({true, false}), ⊆) abstract domain.
case class Bool( bs:Set[Boolean] ) extends Value {

  override def ∧( v:Value ): Value = {
    v match {
      case b: Bool => Bool(for ( x ← bs ; y ← b.bs ) yield (x && y))
      case _ => sys.error("undefined behavior")
    }

  }
  override def ∨( v:Value ): Value = {
    v match {
      case b: Bool => Bool(for ( x ← bs ; y ← b.bs ) yield (x || y))
      case _ => sys.error("undefined behavior")
    }
  }
  override def ≈( v:Value ): Value = {
    v match {
      case b: Bool => Bool(for ( x ← bs ; y ← b.bs ) yield (x == y))
      case _ => Bool(Set(false))
    }
  }
  override def ≠( v:Value ): Value = {
    v match {
      case b: Bool => Bool(for ( x ← bs ; y ← b.bs ) yield (x != y))
      case _ => Bool(Set(true))
    }
  }

  override def toString =
    if ( bs.size == 1 ) bs.head.toString
    else "{true, false}"
}

object Bool {
  val ⊤ = Bool(Set(true, false))
  val ⊥ = Bool(Set())

  def α( bs:Set[Boolean] ): Bool =
    Bool(bs)
}

// for strings we'll use the {⊥,⊤} domain s.t. ⊥ means no string and ⊤
// means any string, so the ordering is ⊥ ⊑ ⊤.
sealed abstract class Str extends Value
// ...

object Str {
  val ⊤ = // ...
  val ⊥ = // ...

  def α( strs:Set[String] ): Str =
    // ...
}

// for convenience we'll keep a set of addresses and separately a
// boolean indicating whether the reference could also be Null.
case class Reference( as:Set[Address], nil:Boolean = false ) extends Value {
  // ...

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
  val ⊥ = // ...
  val Null = // ...

  def apply( a:Address ): Reference =
    // ...
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
    // ...
  }

  def apply( x:Var ): Value =
    flds(x)

  def +( xv:(Var, Value) ): Object =
    Object( cn, flds + xv )
}

//——————————————————————————————————————————————————————————————————————————————
// Kont

sealed abstract class Kont
case class StmtK( s:Stmt ) extends Kont
case class WhileK( e:Exp, ss:Seq[Stmt] ) extends Kont
case class RetK( x:Var, e:Exp, ρ:Locals ) extends Kont
case class FinK( a:Address ) extends Kont
