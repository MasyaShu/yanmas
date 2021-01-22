package ru.itterminal.botdesk.tickets.controller.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDtoNew;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateFilterDto;

@Component
public class SortFieldsValidator implements Validator {

    private Object target;

    @Override
    public boolean supports(Class<?> clazz) {
        return BaseFilterDtoNew.class.isAssignableFrom(clazz);
    }

    @Override
    public void validate(Object target, Errors errors) {
        target1 = target;
        var classTarget = target.getClass();
        var filterDto = classTarget.cast(target);


        errors.rejectValue("sortByFields", "sortByFields", "INVALID_COMPARISON");
    }
}
