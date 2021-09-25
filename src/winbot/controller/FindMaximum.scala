package winbot.controller

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

/**
 * Used for finding maximum output value for a function
 * @author Mikko Hilpinen
 * @since 19.9.2021, v0.1
 */
object FindMaximum
{
	/**
	 * Finds the maximum value of a function. Expects a 2nd power function / a parable pointing upwards
	 * @param min Minimum input value
	 * @param max Maximum input value
	 * @param stepIncrease A recognizable increase in input
	 * @param maxIterations Maximum number of iterations for searching the target range (default = 5)
	 * @param f The tested function
	 * @param ordering Ordering for function results
	 * @tparam A Type of function result
	 * @return The max input and output of that function
	 */
	def ofParable[A](min: Double, max: Double, stepIncrease: Double, maxIterations: Int = 5)(f: Double => A)
	            (implicit ordering: Ordering[A]) =
		iteration[A](min, max, maxIterations, stepIncrease)(f)
	
	@tailrec
	private def iteration[A](min: Double, max: Double, remainingIterations: Int, stepIncrease: Double)(f: Double => A)
	                        (implicit ordering: Ordering[A]): (Double, A) =
	{
		val totalLength = max - min
		// Case: Searching for a proper range
		if (remainingIterations > 1 && totalLength > stepIncrease * 5)
		{
			// Take two points around the middle and calculate the increase / derivative
			val center = min + totalLength / 2
			val input2 = center + stepIncrease
			
			val centerOutput = f(center)
			val output2 = f(input2)
			
			// Case: The curve is increasing around the center => moves to the right
			if (output2 > centerOutput)
				iteration(center, max, remainingIterations - 1, stepIncrease)(f)
			// Case: The curve is decreasing around the center => moves to the left
			else if (output2 < centerOutput)
				iteration(min, input2, remainingIterations - 1, stepIncrease)(f)
			// Case: There is no measurable difference => considers that the maximum
			else
				center -> centerOutput
		}
		// Case: Determining the best value in the final range
		else
		{
			val finalStep = stepIncrease.max(totalLength / 5)
			Iterator.iterate(min) { _ + finalStep }.takeWhile { _ <= max }.map { in => in -> f(in) }.maxBy { _._2 }
		}
	}
}
