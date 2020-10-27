package ru.itterminal.botdesk.commons.config;

import org.springframework.context.support.ResourceBundleMessageSource;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import ru.itterminal.botdesk.commons.exception.RestExceptionHandler;

/**
 * Common unit-test configuration for rest controllers.
 *
 */
public final class WebTestConfig {

    private WebTestConfig() {
    }

    public static MappingJackson2HttpMessageConverter jacksonDateTimeConverter() {
        ObjectMapper objectMapper = new ObjectMapper();

        objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        objectMapper.registerModule(new JavaTimeModule());

        MappingJackson2HttpMessageConverter converter = new MappingJackson2HttpMessageConverter();
        converter.setObjectMapper(objectMapper);
        return converter;
    }

    public static Object controllerAdvice() {
        return new RestExceptionHandler(getResourceBundleMessageSource());
    }

    public static ResourceBundleMessageSource getResourceBundleMessageSource() {
        ResourceBundleMessageSource source = new ResourceBundleMessageSource();
        source.setBasenames("i18n/messages");
        return source;
    }
}
