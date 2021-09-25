package winbot.model

/**
 * Represents a swap using a liquidity pool
 * @author Mikko Hilpinen
 * @since 19.9.2021, v0.1
 */
case class Swap(pool: LiquidityPool, input: TokenPool)
