package winbot.model

import utopia.flow.datastructure.immutable.Graph.GraphViewEdge

/**
 * Used for calculating trades when using multiple routes
 * @author Mikko Hilpinen
 * @since 20.9.2021, v0.1
 */
case class SwapRoutes(data: Set[Vector[GraphViewEdge[Token, LiquidityPool]]])
{

}
