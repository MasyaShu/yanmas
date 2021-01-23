package ru.itterminal.botdesk.commons.model.filter;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

@TestInstance(PER_CLASS)
class StringFilterTest {

    private static final String IS_EMPTY = "is_empty";
    private static final String TEXT_CONTAINS = "text_contains";
    private static final String STRING_20_CHARACTERS = "string 20 characters";
    StringFilter stringFilter;
    int max = Integer.MAX_VALUE;
    int min = 0;
    String regexp = "";

    @BeforeEach
    void setUp() {
        stringFilter = StringFilter.builder()
                .value(STRING_20_CHARACTERS)
                .typeComparison(IS_EMPTY)
                .build();
    }

    @Test
    void isValid_shouldGetTrue_whenFilterValid() {
        assertTrue(stringFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_TEXT_CONTAINS_ValueEmpty() {
        stringFilter.setTypeComparison(TEXT_CONTAINS);
        stringFilter.setValue("");
        assertThrows(IllegalArgumentException.class,
                () -> stringFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_TEXT_CONTAINS_ValueNull() {
        stringFilter.setTypeComparison(TEXT_CONTAINS);
        stringFilter.setValue(null);
        assertThrows(IllegalArgumentException.class,
                () -> stringFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_TEXT_CONTAINS_Max15Characters() {
        max = 15;
        stringFilter.setTypeComparison(TEXT_CONTAINS);
        assertThrows(IllegalArgumentException.class,
                () -> stringFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_TEXT_CONTAINS_Min25Characters() {
        min = 25;
        stringFilter.setTypeComparison(TEXT_CONTAINS);
        assertThrows(IllegalArgumentException.class,
                () -> stringFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_TEXT_CONTAINS_RegexpNotMatcher() {
        regexp = "NoString";
        stringFilter.setTypeComparison(TEXT_CONTAINS);
        assertThrows(IllegalArgumentException.class,
                () -> stringFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetTrue_whenRegexpMatcher() {
        regexp = STRING_20_CHARACTERS;
        stringFilter.setTypeComparison(TEXT_CONTAINS);
        assertTrue(stringFilter.IsValid(max, min, regexp));
    }


}