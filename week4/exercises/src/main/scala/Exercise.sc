import com.sun.javaws.exceptions.InvalidArgumentException

abstract class Nat {
  def value: Int

  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat = new Succ(this, value + 1)

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

object Zero extends Nat {
  def value: Int = 0

  def isZero: Boolean = true

  def predecessor: Nat = throw new Error("0.predecessor")

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = if (that.isZero) this else throw new Error("0.-")
}

class Succ(n: Nat, v: Int) extends Nat {

  def value: Int = v

  def isZero: Boolean = false

  def predecessor: Nat = n


  def +(that: Nat): Nat = {
    if(that.isZero) this
    else this.successor.+(that.predecessor)
  }

  def -(that: Nat): Nat = {
    if (that.isZero) this
    else this.predecessor.-(that.predecessor)
  }
}

Zero.isZero

Zero.successor.isZero

Zero.successor.+(Zero.successor).value

Zero.successor.successor.-(Zero).value

Zero.successor.successor.-(Zero.successor.successor).value

Zero.value

Zero.successor.value

Zero.successor.predecessor.value

Zero.+(Zero).value

Zero.+(Zero.successor).value


