package ru.itterminal.botdesk.commons.exception;


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
