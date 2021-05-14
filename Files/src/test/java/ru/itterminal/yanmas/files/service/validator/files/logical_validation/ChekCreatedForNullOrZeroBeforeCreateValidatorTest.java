package ru.itterminal.yanmas.files.service.validator.files.logical_validation;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.files.model.File;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;
import static ru.itterminal.yanmas.files.service.validator.files.logical_validation.ChekNameFileForNullOrEmptyBeforeCreateValidator.FILE_NAME;

@SpringJUnitConfig(value = {ChekCreatedAtForNullOrZeroBeforeCreateValidator.class})
class ChekCreatedForNullOrZeroBeforeCreateValidatorTest {

    @Autowired
    ChekCreatedAtForNullOrZeroBeforeCreateValidator validator;

    @ParameterizedTest(name = "{index} createdAt: {0}")
    @NullSource
    @ValueSource(longs = {0})
    void beforeCreate_shouldGetLogicalValidationException_whenFilenameIsNull(Long createdAt) {
        var errors = createMapForLogicalErrors();
        File file = File.builder()
                .size(1000)
                .fileName(FILE_NAME)
                .createdAt(createdAt)
                .build();
        assertEquals(0, errors.values().size());
        validator.logicalValidationBeforeCreate(file, errors);
        assertEquals(1, errors.values().size());
    }


}