package ru.itterminal.yanmas.files.service.validator.files.logical_validation;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.files.model.File;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;

@SpringJUnitConfig(value = {ChekNameFileForNullOrEmptyBeforeCreateValidator.class})
class ChekNameFileForNullOrEmptyBeforeCreateValidatorTest {

    @Autowired
    ChekNameFileForNullOrEmptyBeforeCreateValidator validator;

    @ParameterizedTest(name = "{index} fileName: {0}")
    @NullAndEmptySource
    void beforeCreate_shouldGetLogicalValidationException_whenFilenameIsNull(String fileName) {
        var errors = createMapForLogicalErrors();
        File file = File.builder()
                .size(1000)
                .fileName(fileName)
                .build();
        assertEquals(0, errors.values().size());
        validator.logicalValidationBeforeCreate(file, errors);
        assertEquals(1, errors.values().size());
    }
}