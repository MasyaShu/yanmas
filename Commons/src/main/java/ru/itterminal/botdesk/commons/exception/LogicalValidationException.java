package ru.itterminal.botdesk.commons.exception;

import java.util.List;
import java.util.Map;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

import lombok.Getter;
import lombok.Setter;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;

/**
 * Form validation failed. Contain details of failure.
 *
 * @author Bogdan Saikevych
 */
@Getter
@Setter
@ResponseStatus(HttpStatus.BAD_REQUEST)
public class LogicalValidationException extends RuntimeException {

    private List<ValidationError> warnings;
    private Map<String, List<ValidationError>> fieldErrors;
    private String exceptionCode;

    public LogicalValidationException(String message) {
        super(message);
    }

    public LogicalValidationException(String message, List<ValidationError> warnings) {
        super(message);
        this.warnings = warnings;
    }

    public LogicalValidationException(String message, String exceptionCode, List<ValidationError> warnings) {
        super(message);
        this.exceptionCode = exceptionCode;
        this.warnings = warnings;
    }

    public LogicalValidationException(String exceptionCode, Map<String, List<ValidationError>> fieldErrors) {
        this.exceptionCode = exceptionCode;
        this.fieldErrors = fieldErrors;
    }

}
