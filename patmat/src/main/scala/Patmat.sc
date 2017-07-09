object Patmat {
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }

  val list: List[Int] = insert(3, List(1, 2, 4, 5))

  // Peano numbers
  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat

    def successor: Nat = new Succ(this)

    def + (that: Nat): Nat
    def - (that: Nat): Nat
  }

  object Zero extends Nat {
    override def isZero: Boolean = true

    override def predecessor: Nat = throw sys.error("0.predecessor")

    override def +(that: Nat): Nat = that

    override def -(that: Nat): Nat = if (that.isZero) this else throw sys.error("0.minus")
  }

  class Succ(n: Nat) extends Nat {
    override def isZero: Boolean = false

    override def predecessor: Nat = n

    override def +(that: Nat): Nat = new Succ(n + that)

    override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
  }

  val zero = Zero
  val one = new Succ(zero)
  val two = new Succ(one)
  val four = new Succ(one + two)
  val three = four.predecessor
  val fourMinusThree = four - three
}