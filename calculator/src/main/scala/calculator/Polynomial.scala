package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      Math.pow(b(), 2) - 4*(a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def quadraticFormula(a: Double, b: Double, c: Double): (Double, Double) =
      ((-b + (Math.pow(b, 2) - 4*a*c))/(2*a),
       (-b - (Math.pow(b, 2) - 4*a*c))/(2*a))
    Signal {
      val discriminant = computeDelta(a, b, c)()
      if (discriminant < 0) Set[Double]()
      else if (discriminant == 0) Set[Double](0)
      else {
        val roots = quadraticFormula(a(), b(), c())
        Set[Double](roots._1, roots._2)
      }
    }
  }
}
