package ru.itterminal.yanmas.aau.service.validator.property_values.logical_validation;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.model.Property;
import ru.itterminal.yanmas.aau.model.PropertyValues;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createMapForLogicalErrors;


@SuppressWarnings({"unused"})
@SpringJUnitConfig(value = {CheckIfTheValueMatchesThePropertyTypeValidator.class})
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class CheckIfTheValueMatchesThePropertyTypeValidatorTest {
    @Autowired
    CheckIfTheValueMatchesThePropertyTypeValidator checkIfTheValueMatchesThePropertyTypeValidator;

    @ParameterizedTest(name = "{index} value: {0}")
    @MethodSource("getValuesFromBoolean")
    void checkingAStringForBooleanType(String value) {
        var errors = createMapForLogicalErrors();
        var property = Property.builder()
                .typeProperty("boolean")
                .build();
        var propertyValue = PropertyValues.builder()
                .property(property)
                .value(value)
                .build();
        checkIfTheValueMatchesThePropertyTypeValidator.logicalValidationBeforeCreate(propertyValue, errors);
        assertEquals(0, errors.size());
    }

    @ParameterizedTest(name = "{index} value: {0}")
    @MethodSource("getValuesFromNumber")
    void checkingAStringForNumberType(String value) {
        var errors = createMapForLogicalErrors();
        var property = Property.builder()
                .typeProperty("number")
                .build();
        var propertyValue = PropertyValues.builder()
                .property(property)
                .value(value)
                .build();
        checkIfTheValueMatchesThePropertyTypeValidator.logicalValidationBeforeCreate(propertyValue, errors);
        assertEquals(0, errors.size());
    }

    @ParameterizedTest(name = "{index} value: {0}")
    @MethodSource("getNotValidValuesFromNumber")
    void errorCheckingAStringForNumberType_whenValueNullOrEmpty(String value) {
        var errors = createMapForLogicalErrors();
        var property = Property.builder()
                .typeProperty("number")
                .build();
        var propertyValue = PropertyValues.builder()
                .property(property)
                .value(value)
                .build();
        checkIfTheValueMatchesThePropertyTypeValidator.logicalValidationBeforeCreate(propertyValue, errors);
        assertEquals(1, errors.size());
    }

    @ParameterizedTest(name = "{index} value: {0}")
    @MethodSource("getNotValidValuesFromBoolean")
    void errorCheckingAStringForBooleanType_whenValueNullOrEmpty(String value) {
        var errors = createMapForLogicalErrors();
        var property = Property.builder()
                .typeProperty("boolean")
                .build();
        var propertyValue = PropertyValues.builder()
                .property(property)
                .value(value)
                .build();
        checkIfTheValueMatchesThePropertyTypeValidator.logicalValidationBeforeCreate(propertyValue, errors);
        assertEquals(1, errors.size());
    }

    private static Stream<Arguments> getValuesFromBoolean() {
        return Stream.of(
                Arguments.of("true"),
                Arguments.of("false"),
                Arguments.of("True"),
                Arguments.of("False")
        );
    }

    private static Stream<Arguments> getValuesFromNumber() {
        return Stream.of(
                Arguments.of("1"),
                Arguments.of("1.1"),
                Arguments.of("1"),
                Arguments.of(".0"),
                Arguments.of("0."),
                Arguments.of(Integer.toString(Integer.MAX_VALUE)),
                Arguments.of(Integer.toString(Integer.MIN_VALUE)),
                Arguments.of(Double.toString(Double.MAX_VALUE)),
                Arguments.of(Double.toString(Double.MIN_VALUE)),
                Arguments.of(Float.toString(Float.MAX_VALUE)),
                Arguments.of(Float.toString(Float.MIN_VALUE)),
                Arguments.of(Long.toString(Long.MAX_VALUE)),
                Arguments.of(Long.toString(Long.MIN_VALUE))
        );
    }

    private static Stream<Arguments> getNotValidValuesFromNumber() {
        return Stream.of(
                Arguments.of("1/"),
                Arguments.of("1,1"),
                Arguments.of(",1"),
                Arguments.of(".0."),
                Arguments.of(",0."),
                Arguments.of("")
        );
    }

    private static Stream<Arguments> getNotValidValuesFromBoolean() {
        return Stream.of(
                Arguments.of("1"),
                Arguments.of("0"),
                Arguments.of("Y"),
                Arguments.of("N"),
                Arguments.of("yes"),
                Arguments.of("no"),
                Arguments.of("trues"),
                Arguments.of("is false"),
                Arguments.of("plus"),
                Arguments.of("")
        );
    }
//TODO заменить на enum number and boolean
}