package winbot.model

import winbot.model.enumeration.Side
import winbot.model.enumeration.Side.{First, Second}

/**
 * Represents two token pools which can be used for swapping between them
 * @author Mikko Hilpinen
 * @since 13.9.2021, v0.1
 */
case class TokenPoolPair(pools: Pair[TokenPool]) extends DirectionalSwappableLike[TokenPoolPair]
{
	// ATTRIBUTES   -----------------------
	
	/**
	 * The total size of this pair when both token pools are combined
	 */
	lazy val size = pools.map { _.amount }.sum
	/**
	 * The constant k for this pair, which is based on the multiplied token counts
	 */
	lazy val k = pools.map { _.amount }.product
	/**
	 * @return Ratio of the first token amount to the second token amount (t1 / t2). The lower the ratio, the better
	 *         the trade from t1 to t2.
	 */
	override lazy val ratio = pools.map { _.amount }.reduce { _ / _ }
	
	
	// COMPUTED ---------------------------
	
	/**
	 * @return The first (left) token pool
	 */
	def pool1 = pools.first
	/**
	 * @return The second (right) token pool
	 */
	def pool2 = pools.second
	
	/**
	 * @return The left side token
	 */
	@deprecated("Please use inputToken instead", "v0.1")
	def token1 = pool1.token
	/**
	 * @return The right side token
	 */
	@deprecated("Please use outputToken instead", "v0.1")
	def token2 = pool2.token
	
	/**
	 * @return A copy of this pair with the tokens swapped
	 */
	def reverse = TokenPoolPair(pools.reverse)
	
	
	// IMPLEMENTED  -----------------------
	
	/**
	 * @return Tokens represented in this pair, ordered
	 */
	override def tokens = pools.map { _.token }
	
	override def swapAmountUntilRatioOf(targetRatio: Double): Double =
	{
		if (targetRatio <= ratio)
			0
		else
			swapAmountUntilRatioOf(targetRatio, First)
	}
	
	override def outputForInput(input: Double) = acquiredPoolFor(input).amount
	override def inputForOutput(output: Double) = requiredPoolFor(output).amount
	
	override def afterSwap(acceptedInput: Double, receivedOutput: Double) =
		TokenPoolPair(Pair(pool1 + acceptedInput, pool2 - receivedOutput))
	
	
	// OTHER    ---------------------------
	
	/**
	 * @param side A side
	 * @return The token pool on that side
	 */
	def apply(side: Side) = pools(side)
	
	/**
	 * Calculates a swap rate for the specified token amount
	 * @param tokenAmount Amount of token put in
	 * @param fromSide Side from which the token is added (default = Left / First)
	 * @return Opposite token pool acquired in return
	 */
	def acquiredPoolFor(tokenAmount: Double, fromSide: Side = Second) =
	{
		// k must remain constant before and after the swap
		// k = x * y, where x and y are token amounts (left & right)
		// Therefore also (x + dx) * (y - dy) = k
		// From this we get: dy = k/(x + dx) - y
		val from = pools(fromSide)
		val to = pools(fromSide.opposite)
		val x = from.amount
		val y = to.amount
		val acquiredAmount = k / (x + tokenAmount) - y
		to.withAmount(acquiredAmount)
	}
	
	/**
	 * Calculates a swap input to acquire the specified token output
	 * @param tokenAmount Amount of token wanted out
	 * @param fromSide Side from which the tokens should be acquired
	 * @return How much tokens need to be taken in
	 */
	def requiredPoolFor(tokenAmount: Double, fromSide: Side = Second) =
		acquiredPoolFor(tokenAmount, fromSide.opposite)
	
	/**
	 * Calculates the amount of swapping from input to output token that needs to happen before the
	 * specified swap ratio is reached
	 * @param targetRatio Targeted swap ratio
	 * @param inputSide Side to which tokens are added
	 * @return Amount of tokens that need to be added to that side
	 */
	def swapAmountUntilRatioOf(targetRatio: Double, inputSide: Side) =
	{
		// Uses: R (ratio) = x / y
		// By adding y = k / x, we get => R = x^2 / k (This is because y changes as x changes)
		// Now to find adjustment to x (dx) that makes R become the target ratio Rt
		// (x + dx)^2 / k = Rt
		// => dx = sqrt(Rt * k - x^2)
		val x = pools(inputSide).amount
		// TODO: Add tests for negative values
		Math.sqrt(targetRatio * k - Math.pow(x, 2))
	}
}
