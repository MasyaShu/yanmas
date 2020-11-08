package ru.itterminal.botdesk.commons.exception;

public class FailedSaveEntityException extends RuntimeException {
    public FailedSaveEntityException(String msg) {
        super(msg);
    }
}
