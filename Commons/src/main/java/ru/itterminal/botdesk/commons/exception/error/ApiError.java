package ru.itterminal.botdesk.commons.exception.error;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.springframework.http.HttpStatus;
import org.springframework.web.context.request.ServletWebRequest;
import org.springframework.web.context.request.WebRequest;

import lombok.Data;

@Data
public class ApiError {
    private int status;
    private String title;

    private String detail;
    private long timestamp;
    private String path;

    private String type;
    private Map<String, List<ValidationError>> errors = new HashMap<>();

    public ApiError(HttpStatus status, String title, String detail, String type) {
        this.status = status.value();
        this.title = title;
        this.detail = detail;
        this.type = type;
        this.timestamp = new Date().getTime();
    }

    public ApiError(HttpStatus status, String title, Exception cause) {
        this(status, title, cause.getMessage(), cause.getClass().getName());
    }

    public ApiError withRequest(HttpServletRequest request) {
        setPath(request.getRequestURI());
        return this;
    }

    public ApiError withRequest(WebRequest request) {
        setPath(((ServletWebRequest) request).getRequest().getRequestURI());
        return this;
    }

    public ApiError withDetail(String detail) {
        setDetail(detail);
        return this;
    }
}
