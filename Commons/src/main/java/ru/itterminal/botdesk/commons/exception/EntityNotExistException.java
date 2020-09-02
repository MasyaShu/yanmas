package ru.itterminal.botdesk.commons.exception;


/**
 *
 * e.g. if entity don't exist in database
 *
 */
public class EntityNotExistException extends RuntimeException {

    public EntityNotExistException() {
        super();
    }

    public EntityNotExistException(final String message) {
        super(message);
    }

    public EntityNotExistException(final Throwable cause) {
        super(cause);
    }

    public EntityNotExistException(final String message, final Throwable cause) {
        super(message, cause);
    }

}
