package ru.itterminal.botdesk.commons.exception;

public class FailedSaveEntityException extends RuntimeException {
    public FailedSaveEntityException(String msg, Throwable t) {
        super(msg, t);
    }

    public FailedSaveEntityException(String msg) {
        super(msg);
    }
}
