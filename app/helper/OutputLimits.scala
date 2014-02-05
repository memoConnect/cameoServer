package helper

/**
 * User: Björn Reimer
 * Date: 1/20/14
 * Time: 5:53 PM
 */

object OutputLimits {

  def applyLimits[A](data: Seq[A], offset: Int, limit: Int): Seq[A] = {

    def positiveOrZero(i: Int) = if (i > 0) i else 0

    val start = positiveOrZero(math.min(offset, data.size - 1))
    val end = positiveOrZero(
      limit match {
        case 0 => data.size
        case _ => math.min(start + limit, data.size)
      })

    data.slice(start, end)
  }

}
