package ru.itterminal.yanmas.aau.service.validator.group;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.repository.GroupRepository;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;

import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.addValidationErrorIntoErrors;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.checkStringForEquals;

@Component
@RequiredArgsConstructor
public class LogicalValidationBeforeCreateCheckUniquesGroup implements EntityValidator<Group> {

    public static final String NAME = "name";
    private final GroupRepository repository;


    @Override
    public void logicalValidationBeforeCreate(Group entity,
                                              Map<String, List<ValidationError>> errors) {
        var foundGroup = repository.getByNameAndIsInnerAndAccount_Id(
                entity.getName(),
                entity.getIsInner(),
                entity.getAccount().getId()
        );
        if (!foundGroup.isEmpty()) {
            addValidationErrorIntoErrors(
                    NOT_UNIQUE_CODE,
                    format(NOT_UNIQUE_MESSAGE, THIS_NAME),
                    errors
            );
        }
    }

}
