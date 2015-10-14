package cs260.lwnn.concrete.domains

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
    Locals( x2val + xv )
  }
}

//——————————————————————————————————————————————————————————————————————————————
// Heap

case class Heap( addr2obj:Map[Address, Object] ) {
  def apply( addr:Address ): Object =
    addr2obj(addr)

  def +( xv:(Address, Object) ): Heap = {
    Heap( addr2obj + xv )
  }
}

//——————————————————————————————————————————————————————————————————————————————
// Value

sealed abstract class Value {
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

case class Bool( n:Boolean ) extends Value {
  // All other operations are not supported for Bool

  override def ∧( v:Value ) = {
    v match {
      case b: Bool =>
        Bool(n&&b.n)
      case _ => sys.error("undefined behavior")
    }
  }

  override def ∨( v:Value ) = {
    v match {
      case b: Bool =>
        Bool(n||b.n)
      case _ => sys.error("undefined behavior")
    }
  }

  override def ≈( v:Value ) = {
    v match {
      case b: Bool =>
        Bool(n == b.n)
      case _ => sys.error("undefined behavior")
    }
  }

  override def ≠( v:Value ) = {
    v match {
      case b: Bool =>
        Bool(n != b.n)
      case _ => sys.error("undefined behavior")
    }
  }

  override def toString =
    n.toString
}

case class ℤ( n:BigInt ) extends Value {

  override def +( v:Value ) = {
    v match {
      case z: ℤ =>
        ℤ(n + z.n)
      case _ => sys.error("undefined behavior")
    }
  }

  override def −( v:Value ) = {
    v match {
      case z: ℤ =>
        ℤ(n - z.n)
      case _ => sys.error("undefined behavior")
    }
  }

  override def ×( v:Value ) = {
    v match {
      case z: ℤ =>
        ℤ(n * z.n)
      case _ => sys.error("undefined behavior")
    }
  }

  override def ÷( v:Value ) = {
    v match {
      case z: ℤ => {
        //Check for divide by zero
        if( z.n != 0) {
          ℤ(n / z.n)
        } else {
          sys.error("Trying to divide by Zero: Undefined Behavior")
        }
      }
      case _ => sys.error("undefined behavior")
    }
  }

  override def <( v:Value ) = {
    v match {
      case z: ℤ =>
        Bool(n < z.n)
      case _ => sys.error("undefined behavior")
    }
  }

  override def ≤( v:Value ) = {
    v match {
      case z: ℤ =>
        Bool(n <= z.n)
      case _ => sys.error("undefined behavior")
    }
  }

  override def ≈( v:Value ) = {
    v match {
      case z: ℤ =>
        Bool(n == z.n)
      case _ => sys.error("undefined behavior")
    }
  }

  override def ≠( v:Value ) = {
    v match {
      case z: ℤ =>
        Bool(n != z.n)
      case _ => sys.error("undefined behavior")
    }
  }

  override def toString =
    n.toString
}

case class Str( str:String ) extends Value {
  override def +( v:Value ) = {
    v match {
      case str1: Str =>
        Str(str + str1.str)
      case _ => sys.error("undefined behavior")
    }
  }

  override def <( v:Value ) = {
    v match {
      case str1:Str =>
        Bool(str < str1.str)
      case _ => sys.error("undefined behavior")
    }
  }

  override def ≤( v:Value ) = {
    v match {
      case str1:Str =>
        Bool(str <= str1.str)
      case _ => sys.error("undefined behavior")
    }
  }

  override def ≈( v:Value ) = {
    v match {
      case str1:Str =>
        Bool(str == str1.str)
      case _ => sys.error("undefined behavior")
    }
  }

  override def ≠( v:Value ) = {
    v match {
      case str1:Str =>
        Bool(str != str1.str)
      case _ => sys.error("undefined behavior")
    }
  }

  override def toString =
    str
}

sealed abstract class Reference extends Value

case class Address( loc:BigInt ) extends Reference {
  // Note: All other operations are not allowed on Address value

  override def ≈( v:Value ) = {
    v match {
      case addr:Reference => addr match {
        case nonnull:Address => Bool(loc == nonnull.loc)
        case _ => Bool(false)
      }
      case _ => sys.error("undefined behavior")
    }
  }

  override def ≠( v:Value ) = {
    v match {
      case addr:Reference => addr match {
        case nonnull:Address => Bool(loc != nonnull.loc)
        case _ => Bool(true)
      }
      case _ => sys.error("undefined behavior")
    }
  }

  override def toString =
    "addr" + loc
}

object Address {
  // generate fresh addresses on demand
  var genLoc = 0
  def apply(): Address = { genLoc += 1; Address(genLoc-1) }
}

case object Null extends Reference {
  // Note: All other operations are not allowed on Address value

  override def ≈( v:Value ) = {
    v match {
      case addr:Reference => addr match {
        case nonnull:Address => Bool(false)
        case _ => Bool(true)
      }
      case _ => sys.error("undefined behavior")
    }
  }

  override def ≠( v:Value ) = {
    v match {
      case addr:Reference => addr match {
        case nonnull:Address => Bool(true)
        case _ => Bool(false)
      }
      case _ => sys.error("undefined behavior")
    }
  }

  override def toString =
    "null"
}

object defaultvalue{
  def apply(t:Type): Value = t match {
    case IntT => ℤ(0)
    case BoolT => Bool(false)
    case StrT => Str("")
    case _ => Null
  }
}

//——————————————————————————————————————————————————————————————————————————————
// Object

case class Object( className:String, var2value:Map[Var,Value] ) {
  def update(vr:Var, vl:Value): Object = {
    Object(className, var2value + ((vr, vl)))
  }
}

//——————————————————————————————————————————————————————————————————————————————
// Kont

sealed abstract class Kont
case class StmtK( s:Stmt ) extends Kont
case class WhileK( e:Exp, ss:Seq[Stmt] ) extends Kont
case class RetK(v:Var, e:Exp, locs:Locals) extends Kont
