package winbot.model

import winbot.model.enumeration.Side
import winbot.model.enumeration.Side.{First, Second}
import winbot.model.error.IllegalTokenException

object DirectionalSwappableLike
{
	/**
	 * A non-generic version of the DirectionalSwappableLike trait
	 */
	type DirectionalSwappable = DirectionalSwappableLike[DirectionalSwappableLike[_]]
}

/**
 * A common trait for pool pairs or systems that provide swapping between token types
 * @author Mikko Hilpinen
 * @since 20.9.2021, v0.1
 */
trait DirectionalSwappableLike[+Repr]
{
	// ABSTRACT --------------------------
	
	/**
	 * @return The accepted input token (left) followed by the produced output token (right)
	 */
	def tokens: Pair[Token]
	
	/**
	 * @return Ratio of input to output in this system
	 */
	def ratio: Double
	
	/**
	 * Calculates the amount of swapping (from input token to output token) that needs to happen in order for the
	 * ratio to acquire the specified limit
	 * @param targetRatio Targeted swap ratio
	 * @return Amount of swap input required
	 */
	def swapAmountUntilRatioOf(targetRatio: Double): Double
	
	/**
	 * Calculates a swap rate for the specified input token amount
	 * @param input Amount of input tokens provided
	 * @return Amount of output tokens acquired acquired in return
	 */
	def outputForInput(input: Double): Double
	
	/**
	 * Calculates a swap input to acquire the specified token output
	 * @param output The amount of output token wanted
	 * @return How many input tokens need to be taken in
	 */
	def inputForOutput(output: Double): Double
	
	/**
	 * Simulates a swap
	 * @param acceptedInput Input that was provided (should be proportional to output)
	 * @param receivedOutput Output that was received (should be proportional to input)
	 * @return An altered copy of this item that represents state after the swap
	 */
	def afterSwap(acceptedInput: Double, receivedOutput: Double): Repr
	
	
	// COMPUTED --------------------------------
	
	/**
	 * @return The token accepted as input (left)
	 */
	def inputToken = tokens.first
	/**
	 * @return The token produced as output (right)
	 */
	def outputToken = tokens.second
	
	
	// OTHER    --------------------------------
	
	/**
	 * @param tokenType A token type
	 * @return Whether this system accepts or provides that token
	 */
	def contains(tokenType: Token) = tokens.contains(tokenType)
	
	/**
	 * Calculates the amount of token (input or output) that would be acquired or required at the
	 * opposite side of a swap
	 * @param amount Amount provided in (if input) or required out (if output)
	 * @param side Side on which the amount is specified (first for input, second for output)
	 * @return Acquired output (for specified input) or required input (for specified output)
	 */
	def swapRateFrom(amount: Double, side: Side): Double = side match
	{
		case First => outputForInput(amount)
		case Second => inputForOutput(amount)
	}
	
	/**
	 * @param tokens Tokens provided
	 * @return Tokens that would be acquired in return
	 */
	@throws[IllegalTokenException]("If specified token type is not of input nor output token type")
	def swapRateFor(tokens: TokenPool) = this.tokens.sideOf(tokens.token) match
	{
		case Some(inputSide) =>
			this.tokens(inputSide.opposite)(swapRateFrom(tokens.amount, inputSide))
		case None => throw new IllegalTokenException(
			s"${tokens.token} doesn't match accepted input ($inputToken) or output ($outputToken)")
	}
}
