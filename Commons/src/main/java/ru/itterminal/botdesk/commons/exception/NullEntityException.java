package ru.itterminal.botdesk.commons.exception;

/**
 *
 * e.g. if repository layer received  null instead of real entity
 *
 */
public class NullEntityException extends RuntimeException {

    public NullEntityException() {
        super();
    }

    public NullEntityException(final String message) {
        super(message);
    }

    public NullEntityException(final Throwable cause) {
        super(cause);
    }

    public NullEntityException(final String message, final Throwable cause) {
        super(message, cause);
    }
}
