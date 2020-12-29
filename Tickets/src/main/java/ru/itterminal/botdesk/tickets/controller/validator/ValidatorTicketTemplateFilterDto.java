package ru.itterminal.botdesk.tickets.controller.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateFilterDto;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static ru.itterminal.botdesk.tickets.util.TicketConstants.COMPARISON_PATTERN;
import static ru.itterminal.botdesk.tickets.util.TicketConstants.INVALID_COMPARISON;

@Component
public class ValidatorTicketTemplateFilterDto implements Validator {
    @Override
    public boolean supports(Class<?> clazz) {
        return TicketTemplateFilterDto.class.isAssignableFrom(clazz) || TicketTemplateDtoRequest.class.isAssignableFrom(clazz);
    }

    @Override
    public void validate(Object target, Errors errors) {
        if(TicketTemplateDtoRequest.class.isAssignableFrom(target.getClass())){
            return;
        }

        TicketTemplateFilterDto templateFilterDto = ((TicketTemplateFilterDto) target);
        if (templateFilterDto.getComparisonDataEnd() != null && templateFilterDto.getDateEnd() != null) {
            Pattern pattern = Pattern.compile(COMPARISON_PATTERN);
            Matcher matcher = pattern.matcher(templateFilterDto.getComparisonDataEnd());
            if (!matcher.find()) {
                errors.rejectValue("ComparisonDataEnd", "ComparisonDataEnd", INVALID_COMPARISON);
            }
        }

        if (templateFilterDto.getComparisonDataStart() != null && templateFilterDto.getDateStart() != null) {
            Pattern pattern = Pattern.compile(COMPARISON_PATTERN);
            Matcher matcher = pattern.matcher(templateFilterDto.getComparisonDataEnd());
            if (!matcher.find()) {
                errors.rejectValue("ComparisonDataStart", "ComparisonDataStart", INVALID_COMPARISON);
            }
        }
    }
}
