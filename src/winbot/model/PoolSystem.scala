package winbot.model

import utopia.flow.datastructure.immutable.Graph

/**
 * A system consisting of multiple liquidity pools
 * @author Mikko Hilpinen
 * @since 13.9.2021, v0.1
 */
case class PoolSystem(pools: Set[LiquidityPool])
{
	// ATTRIBUTES   --------------------------
	
	lazy val tokens = pools.flatMap { _.tokens }
	
	// Forms a graph based on pool data, using tokens as nodes and pools as edges
	lazy val graph = Graph[Token, LiquidityPool](
		pools.flatMap { pool => Set(pool, pool.reverse) }.map { pool => (pool.token1, pool, pool.token2) })
	
	
	// OTHER    ------------------------------
	
	/*
	private def edgesAccepting(originToken: Token) =
		graph.node(originToken).leavingEdges
	private def edgesProviding(token: Token) = graph.edgesTo(token)
	*/
	
	/**
	 * Finds all shortest routes between the two specified token types
	 * @param startToken The first token
	 * @param endToken The second token
	 * @return The shortest trade routes between these two tokens
	 */
	def routesBetween(startToken: Token, endToken: Token) =
		graph(startToken).shortestRoutesTo(graph(endToken))
	
	/**
	 * @param pool A liquidity pool
	 * @return A copy of this system without that pool included
	 */
	def -(pool: LiquidityPool) = copy(pools = pools.filter { _.address != pool.address })
}
