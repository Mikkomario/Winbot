package winbot.model.enumeration

/**
 * Represents a binary side (Left or Right, 1 or 2)
 * @author Mikko Hilpinen
 * @since 13.9.2021, v0.1
 */
sealed trait Side
{
	/**
	 * @return Index of this side when using arrays
	 */
	def index: Int
	
	/**
	 * @return The side opposite to this one
	 */
	def opposite: Side
}

object Side
{
	// ATTRIBUTES   ---------------------
	
	/**
	 * All 2 values of this enumeration
	 */
	val values = Vector(First, Second)
	
	
	// NESTED   -------------------------
	
	/**
	 * The first / left side
	 */
	case object First extends Side
	{
		override def index = 0
		override def opposite = Second
	}
	
	/**
	 * The second / right side
	 */
	case object Second extends Side
	{
		override def index = 1
		override def opposite = First
	}
}