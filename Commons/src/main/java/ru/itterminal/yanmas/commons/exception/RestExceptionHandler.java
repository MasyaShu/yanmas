package ru.itterminal.yanmas.commons.exception;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.validation.ConstraintViolationException;

import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.validation.FieldError;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import org.springframework.web.multipart.MaxUploadSizeExceededException;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import io.jsonwebtoken.JwtException;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.commons.exception.error.ApiError;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;

@SuppressWarnings("NullableProblems")
@Slf4j
@ControllerAdvice
@NoArgsConstructor
public class RestExceptionHandler extends ResponseEntityExceptionHandler {

    private static final String INPUT_VALIDATION_FAILED = "Input Validation Failed";
    public static final String ACCESS_IS_DENIED = "Access is denied";

    @ExceptionHandler(OptimisticLockingFailureException.class)
    public ResponseEntity<?> handleOptimisticLockingFailureException(OptimisticLockingFailureException ex, HttpServletRequest request) {
        var apiError = new ApiError(HttpStatus.CONFLICT, "OptimisticLocking", ex).withRequest(request);
        log.warn(ex.getMessage());
        return new ResponseEntity<>(apiError, HttpStatus.CONFLICT);
    }

    @ExceptionHandler(MethodArgumentTypeMismatchException.class)
    public ResponseEntity<?> handleMethodArgumentTypeMismatchException(MethodArgumentTypeMismatchException ex, HttpServletRequest request) {
        var apiError = new ApiError(HttpStatus.BAD_REQUEST, "Request not readable", ex).withRequest(request);
        log.warn(ex.getMessage());
        return new ResponseEntity<>(apiError, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(UnsupportedOperationException.class)
    public ResponseEntity<?> handleJwtException(UnsupportedOperationException ex, HttpServletRequest request) {
        var apiError = new ApiError(HttpStatus.METHOD_NOT_ALLOWED, "Error", ex).withRequest(request);
        log.warn(ex.getMessage());
        return new ResponseEntity<>(apiError, HttpStatus.METHOD_NOT_ALLOWED);
    }

    @ExceptionHandler(JwtException.class)
    public ResponseEntity<?> handleJwtException(JwtException ex, HttpServletRequest request) {
        var apiError = new ApiError(HttpStatus.BAD_REQUEST, "JWT", ex).withRequest(request);
        log.warn(ex.getMessage());
        return new ResponseEntity<>(apiError, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler({FailedSaveEntityException.class})
    public ResponseEntity<?> handleFailedSaveEntityException(FailedSaveEntityException ex, HttpServletRequest request) {
        var apiError = new ApiError(HttpStatus.CONFLICT, "Save entity", ex).withRequest(request);
        log.warn(ex.getMessage());
        return new ResponseEntity<>(apiError, HttpStatus.CONFLICT);
    }

    @ExceptionHandler(LogicalValidationException.class)
    public ResponseEntity<Object> handleLogicalValidationException(LogicalValidationException ex, HttpServletRequest request) {
        var apiError = new ApiError(
            HttpStatus.CONFLICT,
            ex.getExceptionCode(), ex)
            .withRequest(request)
            .withDetail(INPUT_VALIDATION_FAILED);
        if (ex.getFieldErrors() != null) {
            apiError.setErrors(ex.getFieldErrors());
        }
        log.warn(ex.getMessage());
        return new ResponseEntity<>(apiError, null, HttpStatus.CONFLICT);
    }


    @Override
    protected ResponseEntity<Object> handleHttpRequestMethodNotSupported(HttpRequestMethodNotSupportedException ex,
            HttpHeaders headers, HttpStatus status, WebRequest request) {
        var apiError = new ApiError(status, "Unsupported http method", ex).withRequest(request);
        log.warn(ex.getMessage());
        return handleExceptionInternal(ex, apiError, headers, HttpStatus.METHOD_NOT_ALLOWED, request);
    }

    @Override
    protected ResponseEntity<Object> handleHttpMessageNotReadable(
        HttpMessageNotReadableException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        var apiError = new ApiError(status, "Message Not Readable", ex).withRequest(request);
        log.warn(ex.getMessage());
        return handleExceptionInternal(ex, apiError, headers, status, request);
    }

    @Override
    protected ResponseEntity<Object> handleMethodArgumentNotValid(
        MethodArgumentNotValidException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        var apiError = new ApiError(
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

    @ExceptionHandler({ConstraintViolationException.class})
    public ResponseEntity<ApiError> handleConstraintViolations(Exception ex, WebRequest request) {
        var apiError = new ApiError(
            HttpStatus.BAD_REQUEST,
            "Request not readable", ex)
            .withRequest(request);
        log.warn(ex.getMessage());
        return new ResponseEntity<>(apiError, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(JwtAuthenticationException.class)
    public ResponseEntity<ApiError> handleJwtAuthenticationException(JwtAuthenticationException ex, HttpServletRequest request) {
        var apiError = new ApiError(HttpStatus.FORBIDDEN, "authentication failed", ex).withRequest(request);
        return new ResponseEntity<>(apiError, null, HttpStatus.FORBIDDEN);
    }

    @ExceptionHandler(AccessDeniedException.class)
    public ResponseEntity<ApiError> handleAccessDeniedException(AccessDeniedException ex, HttpServletRequest request) {
        var apiError = ApiError.builder()
                .status(HttpStatus.FORBIDDEN.value())
                .title(ACCESS_IS_DENIED)
                .detail(ex.getMessage())
                .type(ex.getClass().getCanonicalName())
                .path("(" + request.getMethod() + ") " + request.getRequestURI())
                .timestamp(new Date().getTime())
                .build();
        return new ResponseEntity<>(apiError, null, HttpStatus.FORBIDDEN);
    }

    @ExceptionHandler(EntityNotExistException.class)
    public ResponseEntity<ApiError> handleEntityNotExistException(EntityNotExistException ex, HttpServletRequest request) {
        var apiError = new ApiError(HttpStatus.NOT_FOUND, "Resource Not Found", ex).withRequest(request);
        return new ResponseEntity<>(apiError, null, HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(MaxUploadSizeExceededException.class)
    public ResponseEntity<?> handleMaxUploadSizeExceededException(MaxUploadSizeExceededException ex, HttpServletRequest request) {
        var apiError = new ApiError(HttpStatus.CONFLICT, "Maximum upload size exceeded", ex).withRequest(request);
        log.warn(ex.getMessage());
        return new ResponseEntity<>(apiError, HttpStatus.CONFLICT);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<Object> handleInternal(final Exception ex, final WebRequest request) {
        logger.error("500 Status Code", ex);
        final String bodyOfResponse = "Internal server error";
        return handleExceptionInternal(ex, bodyOfResponse, new HttpHeaders(), HttpStatus.INTERNAL_SERVER_ERROR, request);
    }

}
