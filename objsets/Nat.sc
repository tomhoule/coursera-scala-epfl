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
  def successor = new Succ(this)
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) this else throw new java.lang.ArithmeticException
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def successor = new Succ(this)
  def + (that: Nat) = if (that.isZero) this else new Succ(that + n)
  def - (that: Nat) = if (that.isZero) this else n - that.predecessor
}


new Succ(new Zero()) + new Succ(new Zero) - new Succ(new Zero)
(new Succ(new Succ(new Zero)) - new Succ(new Zero)) - new Succ(new Zero)