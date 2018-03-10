package score.discord.redditbot

object MathUtil {
  def ciLowerBound(pos: Int, neg: Int, z: Double): Double = {
    val n = pos + neg
    if (n == 0) return 0
    //val z = Statistics2.pnormaldist(1-(1-confidence)/2)
    val phat = 1.0*pos/n
    (phat + z*z/(2*n) - z * Math.sqrt((phat*(1-phat)+z*z/(4*n))/n))/(1+z*z/n)
  }
}
