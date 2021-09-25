package winbot.model

import winbot.model.enumeration.Side
import winbot.model.enumeration.Side.{First, Second}

/**
 * A struct holding two values of the same type
 * @author Mikko Hilpinen
 * @since 13.9.2021, v0.1
 */
// TODO: Replace with the Flow version (extracted from this one)
case class Pair[+A](first: A, second: A) extends Iterable[A]
{
	// ATTRIBUTES   ----------------------
	
	override lazy val toVector = Vector(first, second)
	
	
	// COMPUTED --------------------------
	
	/**
	 * @return A reversed copy of this pair
	 */
	def reverse = Pair(second, first)
	/**
	 * @return A tuple based on this pair
	 */
	def toTuple = first -> second
	/**
	 * @return A map with the same contents with this pair
	 */
	def toMap = Map(First -> first, Second -> second)
	
	
	// IMPLEMENTED  ----------------------
	
	override def iterator = toVector.iterator
	
	override def map[B](f: A => B) = Pair(f(first), f(second))
	
	
	// OTHER    --------------------------
	
	/**
	 * @param side A side
	 * @return The item of this pair from that side
	 */
	def apply(side: Side) = toVector(side.index)
	
	/**
	 * @param item An item
	 * @return Whether this pair contains that specific item
	 */
	def contains[B >: A](item: B) = first == item || second == item
	
	/**
	 * @param item An item
	 * @return The side on which that item resides in this pair. None if that item is not in this pair.
	 */
	def sideOf[B >: A](item: B): Option[Side] =
		if (item == first) Some(First) else if (item == second) Some(Second) else None
}
