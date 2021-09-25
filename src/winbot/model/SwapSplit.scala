package winbot.model

import winbot.model.DirectionalSwappableLike.DirectionalSwappable

/**
 * Represents a "split in the road" of different swap routes that have the same input and output tokens.
 * This piece splits the token inputs so that price impact is avoided.
 * @author Mikko Hilpinen
 * @since 21.9.2021, v0.1
 */
case class SwapSplit(routes: Set[DirectionalSwappable]) extends DirectionalSwappableLike[SwapSplit]
{
	// ATTRIBUTES   ----------------------------
	
	// Routes ordered by ratio, coupled with that ratio
	private lazy val routeRatios = routes.toVector.map { route => route -> route.ratio }.sortBy { _._2 }
	
	/**
	 * @return The route that is preferred (initially)
	 */
	lazy val primaryRoute = routeRatios.head._1
	
	// TODO: Should somehow alter the state of the routes so that they behave as if they had swapped X tokens
	//  (would make calculations much easier)
	private lazy val thresholds = routeRatios.map { case (_, maxRatio) =>
		val readyRoutes = routeRatios.filter { _._2 == maxRatio }.map { _._1 }
		val pushedRoutes = routeRatios.takeWhile { _._2 < maxRatio }.map { _._1 }
		RouteGroup(readyRoutes.map { _ -> 0.0 }.toMap ++
			pushedRoutes.map { route => route -> route.swapAmountUntilRatioOf(maxRatio) }, maxRatio)
	}
	
	
	// COMPUTED --------------------------------
	
	
	// IMPLEMENTED  ----------------------------
	
	// Since the best route may be utilized, that counts as the (starting) ratio for this split
	override def ratio = routeRatios.head._2
	
	override def tokens = primaryRoute.tokens
	
	override def swapAmountUntilRatioOf(targetRatio: Double) =
	{
		// Case: Ratio has already been reached => skips calculations
		if (targetRatio <= ratio)
			0
		// Case: Ratio is yet to be reached =>
		// Combines the required amounts, expecting all of them to be filled before that ratio is reached
		// TODO: Use cached values to limit the amount of calculations required
		else
			routeRatios.takeWhile { _._2 < targetRatio }
				.map { case (route, _) => route.swapAmountUntilRatioOf(targetRatio) }.sum
	}
	
	override def outputForInput(input: Double) = ???
	
	override def inputForOutput(output: Double) = ???
	
	override def afterSwap(acceptedInput: Double, receivedOutput: Double) = ???
}

// TODO: May be unnecessary
private case class RouteGroup(startValues: Map[DirectionalSwappable, Double], startRatio: Double)
{
	lazy val totalStartInput = startValues.valuesIterator.sum
}
