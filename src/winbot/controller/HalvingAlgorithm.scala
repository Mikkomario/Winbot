package winbot.controller

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

/**
 * Finds a local maximum within a function using the halving algorithm
 * @author Mikko Hilpinen
 * @since 13.9.2021, v1.12
 */
object HalvingAlgorithm
{
	/**
	 * Finds a local maximum within a function by halving the input value and comparing
	 * @param min           Minimum input value
	 * @param max           Maximum input value
	 * @param maxIterations Maximum number of iterations to use (default = 10). The greater value causes this function
	 *                      to run longer but can provide more accurate results
	 * @param function      Function to test
	 * @param ordering      Ordering for function result values
	 * @tparam A Type of function result
	 * @return Function input that produced the greatest value + that greatest value
	 */
	def apply[A](min: Double, max: Double, maxIterations: Int = 10)(function: Double => A)
	            (implicit ordering: Ordering[A]) =
		iteration(min, function(min), max, function(max), maxIterations - 1)(function)
	
	@tailrec
	private def iteration[A](minInput: Double, minOutput: A, maxInput: Double, maxOutput: A, remainingIterations: Int)
	                        (function: Double => A)(implicit ordering: Ordering[A]): (Double, A) =
	{
		// Calculates new center value
		val centerInput = (minInput + maxInput) / 2
		val centerOutput = function(centerInput)
		
		if (remainingIterations > 0) {
			// Checks which side is better
			val (newMinIn, newMinOut, newMaxIn, newMaxOut) = {
				if (minOutput < maxOutput)
					(centerInput, centerOutput, maxInput, maxOutput)
				else
					(minInput, minOutput, centerInput, centerOutput)
			}
			// Checks whether maximum accuracy has been acquired
			if (newMinOut == newMaxOut)
				centerInput -> centerOutput
			else
				iteration(newMinIn, newMinOut, newMaxIn, newMaxOut, remainingIterations - 1)(function)
		}
		else
			centerInput -> centerOutput
	}
}
