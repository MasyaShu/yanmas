package ru.itterminal.botdesk.commons.exception;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.validation.ConstraintViolationException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.validation.FieldError;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.exception.error.ApiError;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;

/**
 * Specify how exception should be translated to http response.
 */

@Slf4j
@ControllerAdvice
@NoArgsConstructor
@AllArgsConstructor
public class RestExceptionHandler extends ResponseEntityExceptionHandler {

    public static final String VALIDATION_ERROR_LIST_KEY = "incorrect_fields";
    private static final String INPUT_VALIDATION_FAILED = "Input Validation Failed";

    @Autowired
    private MessageSource messageSource;

//    @ExceptionHandler(InappropriateResourceStateException.class)
//    public ResponseEntity<?> handleInappropriateResourceStateException(InappropriateResourceStateException ex, HttpServletRequest request) {
//        ApiError apiError = new ApiError(HttpStatus.BAD_REQUEST, ex.getExceptionCode(), ex).withRequest(request);
//        log.warn(ex.getMessage());
//        return new ResponseEntity<>(apiError, null, HttpStatus.BAD_REQUEST);
//    }

    @ExceptionHandler(OptimisticLockingFailureException.class)
    public ResponseEntity<?> handleOptimisticLockingFailureException(OptimisticLockingFailureException ex, HttpServletRequest request) {
        ApiError apiError = new ApiError(HttpStatus.CONFLICT, "OptimisticLocking", ex).withRequest(request);
        log.warn(ex.getMessage());
        return new ResponseEntity<>(apiError, HttpStatus.CONFLICT);
    }
//
//    @ExceptionHandler(RelatedEntityMarkedAsDeletedException.class)
//    public ResponseEntity<?> handleLogicalDeletedException(RelatedEntityMarkedAsDeletedException ex, HttpServletRequest request) {
//        ApiError apiError = new ApiError(HttpStatus.CONFLICT, "LogicalDeleted", ex).withRequest(request);
//        log.warn(ex.getMessage());
//        return new ResponseEntity<>(apiError, HttpStatus.CONFLICT);
//    }

    @ExceptionHandler(MethodArgumentTypeMismatchException.class)
    public ResponseEntity<?> handleMethodArgumentTypeMismatchException(MethodArgumentTypeMismatchException ex, HttpServletRequest request) {
        ApiError apiError = new ApiError(HttpStatus.BAD_REQUEST, "Request not readable", ex).withRequest(request);
        log.warn(ex.getMessage());
        return new ResponseEntity<>(apiError, HttpStatus.BAD_REQUEST);
    }

//    @ExceptionHandler(NotUniqueValueException.class)
//    public ResponseEntity<?> handleNotUniqueValueException(NotUniqueValueException ex, HttpServletRequest request) {
//        ApiError apiError = new ApiError(HttpStatus.CONFLICT, ex.getExceptionCode(), ex).withRequest(request);
//        log.warn(ex.getMessage());
//        return new ResponseEntity<>(apiError, null, HttpStatus.CONFLICT);
//    }

    @ExceptionHandler(LogicalValidationException.class)
    public ResponseEntity<Object> handleLogicalValidationException(LogicalValidationException ex, HttpServletRequest request) {
        ApiError apiError = new ApiError(
            HttpStatus.CONFLICT,
            ex.getExceptionCode(), ex)
            .withRequest(request)
            .withDetail(INPUT_VALIDATION_FAILED);
        //TODO: check auth-server to avoid this 'if'-block
        if (ex.getFieldErrors() == null) {
            apiError.setErrors(new HashMap<String, List<ValidationError>>() {{
                put(VALIDATION_ERROR_LIST_KEY, ex.getWarnings());
            }});
        } else {
            apiError.setErrors(ex.getFieldErrors());
        }
        log.warn(ex.getMessage());
        return new ResponseEntity<>(apiError, null, HttpStatus.CONFLICT);
    }


    @Override
    protected ResponseEntity<Object> handleHttpRequestMethodNotSupported(HttpRequestMethodNotSupportedException ex,
            HttpHeaders headers, HttpStatus status, WebRequest request) {
        ApiError apiError = new ApiError(status, "Unsupported http method", ex).withRequest(request);
        log.warn(ex.getMessage());
        return handleExceptionInternal(ex, apiError, headers, HttpStatus.METHOD_NOT_ALLOWED, request);
    }

    @Override
    protected ResponseEntity<Object> handleHttpMessageNotReadable(
        HttpMessageNotReadableException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        ApiError apiError = new ApiError(status, "Message Not Readable", ex).withRequest(request);
        log.warn(ex.getMessage());
        return handleExceptionInternal(ex, apiError, headers, status, request);
    }

    @Override
    protected ResponseEntity<Object> handleMethodArgumentNotValid(
        MethodArgumentNotValidException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        ApiError apiError = new ApiError(
            HttpStatus.BAD_REQUEST,
            "Validation Failed", ex)
            .withRequest(request)
            .withDetail(INPUT_VALIDATION_FAILED);

        // create Validation Error
        List<FieldError> fieldErrorList = ex.getBindingResult().getFieldErrors();
        for (FieldError fieldError : fieldErrorList) {
            // create list if it doesn't exists
            List<ValidationError> errors = apiError.getErrors().computeIfAbsent(
                fieldError.getField(), k -> new ArrayList<>());
            // add the error to the list
            ValidationError e = new ValidationError();
            e.setCode(fieldError.getCode());
            e.setMessage(fieldError.getDefaultMessage());
            errors.add(e);
        }
        log.warn(ex.getMessage());
        return handleExceptionInternal(ex, apiError, headers, status, request);
    }

    @ExceptionHandler(ConstraintViolationException.class)
    public ResponseEntity<ApiError> handleConstraintViolations(ConstraintViolationException ex, WebRequest request) {
        ApiError apiError = new ApiError(
            HttpStatus.BAD_REQUEST,
            "Request not readable", ex)
            .withRequest(request);
        log.warn(ex.getMessage());
        return new ResponseEntity<>(apiError, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(JwtAuthenticationException.class)
    public ResponseEntity<?> handleJwtAuthenticationException(JwtAuthenticationException ex, HttpServletRequest request) {
        ApiError apiError = new ApiError(HttpStatus.FORBIDDEN, "Authentication failed", ex).withRequest(request);
        return new ResponseEntity<>(apiError, null, HttpStatus.FORBIDDEN);
    }

    @ExceptionHandler(EntityNotExistException.class)
    public ResponseEntity<?> handleEntityNotExistException(EntityNotExistException ex, HttpServletRequest request) {
        ApiError apiError = new ApiError(HttpStatus.NOT_FOUND, "Resource Not Found", ex).withRequest(request);
        return new ResponseEntity<>(apiError, null, HttpStatus.NOT_FOUND);
    }

//    @ExceptionHandler(NotUniqueNameException.class)
//    public ResponseEntity<?> handleResourceNotUniqueNameException(NotUniqueNameException ex, HttpServletRequest request) {
//        ApiError apiError = new ApiError(HttpStatus.BAD_REQUEST, "Already exist", ex).withRequest(request);
//        return new ResponseEntity<>(apiError, null, HttpStatus.BAD_REQUEST);
//    }
//
//    @ExceptionHandler(value = {UserAlreadyExistException.class})
//    protected ResponseEntity<Object> UserAlreadyExist(final RuntimeException ex, final WebRequest request) {
//        final String bodyOfResponse = "User already exist";
//        return handleExceptionInternal(ex, bodyOfResponse, new HttpHeaders(), HttpStatus.BAD_REQUEST, request);
//    }
//
//    @ExceptionHandler(Exception.class)
//    public ResponseEntity<Object> handleInternal(final Exception ex, final WebRequest request) {
//        logger.error("500 Status Code", ex);
//
//        final String bodyOfResponse = "Internal server error";
//        return handleExceptionInternal(ex, bodyOfResponse, new HttpHeaders(), HttpStatus.INTERNAL_SERVER_ERROR, request);
//    }
//
//    @ExceptionHandler(value = {DataIntegrityViolationException.class})
//    public ResponseEntity<Object> handleDataIntegrityViolationException(final DataIntegrityViolationException ex, final WebRequest request) {
//        logger.error(ex.getMessage());
//        ApiError apiError = new ApiError(
//            HttpStatus.CONFLICT,
//            "Validation failed", ex)
//            .withRequest(request)
//            .withDetail(INPUT_VALIDATION_FAILED);
//        //TODO: Has to be refactored (what would be when 'if' would be false?)
//        String exMessage = "";
//        if (ex.getCause().getClass().isAssignableFrom(org.hibernate.exception.ConstraintViolationException.class)) {
//            exMessage = ex.getCause().getCause().getMessage();
//            Map<String, List<ValidationError>> errors = new HashMap<>();
//            log.debug("Trying parse exception message: {}", exMessage);
//            int beginIndex = exMessage.indexOf('(') + 1;
//            int endIndex = exMessage.indexOf(')');
//            String substring = "not defined";
//            if (!(endIndex <= 2 || beginIndex < endIndex)) {
//                substring = exMessage.substring(beginIndex, endIndex);
//            } else {
//                log.warn("Parsing exception message was failed: {}", exMessage);
//            }
//            errors.put(substring, singletonList(new ValidationError("not unique",
//                substring + " is occupied")));
//            apiError.setErrors(errors);
//        }
//        return new ResponseEntity<>(apiError, null, HttpStatus.CONFLICT);
//    }
}
