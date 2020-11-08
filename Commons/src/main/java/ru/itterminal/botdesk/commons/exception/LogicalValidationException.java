package ru.itterminal.botdesk.commons.exception;

import java.util.List;
import java.util.Map;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

import lombok.Getter;
import lombok.Setter;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;

@Getter
@Setter
@ResponseStatus(HttpStatus.BAD_REQUEST)
public class LogicalValidationException extends RuntimeException {

    private final Map<String, List<ValidationError>> fieldErrors;
    private final String exceptionCode;

    public LogicalValidationException(String exceptionCode, Map<String, List<ValidationError>> fieldErrors) {
        this.exceptionCode = exceptionCode;
        this.fieldErrors = fieldErrors;
    }

}
