abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

class Zero extends Nat {
  def isZero = true
  def predecessor: Nothing = throw new java.lang.ArithmeticException
  def successor = new One()
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) this else throw new java.lang.ArithmeticException
}

class OneMore(prev: Nat) extends Nat {
  def isZero = false
  def predecessor = prev
  def successor = new OneMore(this)
  def + (that: Nat) = if (that.isZero) this else new OneMore(that + prev)
  def - (that: Nat) = if (that.isZero) this else that - prev
}


class One extends Nat {
  def isZero = false
  def predecessor = new Zero
  def successor = new OneMore(this)
  def + (that: Nat) = if (that.isZero) this else that.successor
  def - (that: Nat) = if (that.isZero) this else that.predecessor
}


new Zero() + new One()
new One() - new Zero() - new One()

new One() + new One() + new One() - new One