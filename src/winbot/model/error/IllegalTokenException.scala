package winbot.model.error

/**
 * Thrown when a wrong type of token is being used
 * @author Mikko Hilpinen
 * @since 20.9.2021, v0.1
 */
class IllegalTokenException(message: String, cause: Throwable = null) extends Exception(message, cause)
