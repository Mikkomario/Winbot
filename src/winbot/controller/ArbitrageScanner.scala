package winbot.controller

import winbot.controller.ArbitrageScanner.swapCost
import winbot.model.enumeration.Side
import winbot.model.{LiquidityPool, PoolSystem, Swap, Token, TokenPool}

object ArbitrageScanner
{
	// These are placeholder values for now
	private val swapCost = 0.2
}

/**
 * Used for searching for arbitrage opportunities
 * @author Mikko Hilpinen
 * @since 13.9.2021, v0.1
 */
class ArbitrageScanner(system: PoolSystem, safeTokens: Map[Token, Double], balance: Map[Token, Double])
{
	def target(pool: LiquidityPool) =
	{
		val systemWithoutPool = system - pool
	}
	
	private def _target(pool: LiquidityPool, system: PoolSystem, targetSide: Side) =
	{
		// Finds routes to the first token in that pool
		val enterToken = pool(targetSide).token
		// Case: The pool accepts one of the safe tokens => uses that directly
		if (safeTokens.contains(enterToken))
		{
			// Finds routes out of the second token in that pool
			val exitToken = pool(targetSide.opposite).token
			// Case: Both the ends of the pool are safe => Checks for a direct trade
			// TODO: The profit must be greater in these ones to avoid stupid trades due to estimated value modifier
			if (safeTokens.contains(exitToken))
			{
				// Checks for a value difference between the tokens
				val valueModifier = modifierFor(enterToken, exitToken)
				// Calculates the profit that can be acquire through trading
				def countProfit(input: Double) = pool.pair.acquiredPoolFor(input).amount * valueModifier - input
				val (maxInput, maxProfit) = optimize(balance(enterToken))(countProfit)
				// If the trade makes sense, prepares it
				if (maxProfit > swapCost)
					Some(Vector(Swap(pool, TokenPool(enterToken, maxInput))))
				else
					None
			}
			// Case: Acquired token needs to be swapped => checks different trade routes
			else
			{
				val allExitRoutes = safeTokens.keySet.map { token => system.routesBetween(exitToken, token) }
					.filter { _.nonEmpty }
				val minExitRouteLength = allExitRoutes.map { _.head.size }.min
				val exitRoutes = allExitRoutes.filter { _.head.size == minExitRouteLength }.flatten
				
				// TODO: Handle case where there is just one route (no need to calculate this stuff)
				// Calculates the swap ratio for each step in the routes.
				// Uses the better ratios until they become equal to the others.
				
				// Calculates the profitability of each route
				// TODO: Has to combine the routes somehow, not just pick one
				//  (alternatively, pick one for calculations and use 1inch or something for the actual trade)
				exitRoutes.map { route =>
					val endToken = route.last.end.content
					val valueModifier = modifierFor(enterToken, endToken)
					def countProfit(input: Double) = {
						val start = pool.pair.acquiredPoolFor(input).amount
						val output = route.foldLeft(start) { (input, edge) =>
							edge.content.acquiredPoolFor(input).amount
						}
						output * valueModifier - input
					}
					val (maxInput, maxProfit) = optimize(balance(enterToken))(countProfit)
					
					???
				}
				???
			}
		}
		// Case: The pool accepts some other token => tests with different trade routes
		else
			???
	}
	
	private def modifierFor(origin: Token, target: Token) = safeTokens(target) / safeTokens(origin)
	
	private def optimize(maxInput: Double)(function: Double => Double) =
	{
		// Calculates the best input value
		FindMaximum.ofParable(0, maxInput, 0.1) { in => function(in) }
	}
}
