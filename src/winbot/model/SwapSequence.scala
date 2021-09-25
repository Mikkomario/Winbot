package winbot.model

import winbot.model.DirectionalSwappableLike.DirectionalSwappable

/**
 * Represents a linear trading route for swapping tokens
 * @author Mikko Hilpinen
 * @since 20.9.2021, v0.1
 */
case class SwapSequence(steps: Vector[DirectionalSwappable]) extends DirectionalSwappableLike[SwapSequence]
{
	// ATTRIBUTES   -----------------------------
	
	// TODO: This is not the true ratio, but a careful estimate (when changing this, change swapAmount... algorithm)
	override lazy val ratio = steps.map { _.ratio }.max
	
	override lazy val tokens = Pair(steps.head.inputToken, steps.last.outputToken)
	
	
	// OTHER    --------------------------------
	
	override def outputForInput(input: Double) =
		steps.foldLeft(input) { (input, step) => step.outputForInput(input) }
	
	override def inputForOutput(output: Double) =
		steps.reverseIterator.foldLeft(output) { (output, step) => step.inputForOutput(output) }
	
	override def swapAmountUntilRatioOf(targetRatio: Double) =
	{
		// For each step, calculates the input required to reach the specified ratio
		// Then converts that input amount to the original token input amount (at step 0)
		steps.indices.map { stepIndex =>
			steps.take(stepIndex).reverseIterator
				.foldLeft(steps(stepIndex).swapAmountUntilRatioOf(targetRatio)) { (maxOutput, step) =>
					step.inputForOutput(maxOutput)
				}
		}.min // Selects the smallest amount (considers ratio reached when ANY of the steps has that ratio)
	}
	
	override def afterSwap(acceptedInput: Double, receivedOutput: Double) = ???
}
