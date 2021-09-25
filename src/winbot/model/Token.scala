package winbot.model

/**
 * Represents a tradeable token which has some value
 * @author Mikko Hilpinen
 * @since 13.9.2021, v0.1
 */
case class Token(name: String, address: String)
{
	/**
	 * @param amount Amount of this token
	 * @return A pool with that much of this token
	 */
	def apply(amount: Double) = TokenPool(this, amount)
}
