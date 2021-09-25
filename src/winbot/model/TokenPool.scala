package winbot.model

import utopia.flow.operator.{Combinable, LinearScalable}

/**
 * Represents 1-n tokens of a single type
 * @author Mikko Hilpinen
 * @since 13.9.2021, v0.1
 */
case class TokenPool(token: Token, amount: Double) extends LinearScalable[TokenPool] with Combinable[TokenPool, Double]
{
	// IMPLEMENTED  -----------------------------
	
	override def +(other: Double) = withAmount(amount + other)
	
	override def *(mod: Double) = withAmount(amount * mod)
	
	
	// OTHER    ---------------------------------
	
	def -(amount: Double) = withAmount(this.amount - amount)
	
	/**
	 * @param amount A new token amount
	 * @return A copy of this pool with that token amount
	 */
	def withAmount(amount: Double) = copy(amount = amount)
}
