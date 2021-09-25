package winbot.model

import utopia.flow.util.Extender

/**
 * Represents a liquidity pool used for trading assets
 * @author Mikko Hilpinen
 * @since 13.9.2021, v0.1
 */
case class LiquidityPool(name: String, address: String, pair: TokenPoolPair) extends Extender[TokenPoolPair]
{
	// COMPUTED --------------------------------
	
	/**
	 * @return A reversed copy of this pool where token1 and token2 have switched places
	 */
	// TODO: Should remember that this pool is reversed (affects smart contract calling)
	def reverse = copy(pair = pair.reverse)
	
	
	// IMPLEMENTED  ----------------------------
	
	override def wrapped = pair
}