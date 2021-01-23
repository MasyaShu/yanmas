package ru.itterminal.botdesk.commons.model.filter;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

@TestInstance(PER_CLASS)
class BooleanFilterTest {

    BooleanFilter booleanFilter;
    int max = Integer.MAX_VALUE;
    int min = 0;
    String regexp = "";

    @BeforeEach
    void setUp() {
        booleanFilter = BooleanFilter.builder()
                .value(true)
                .build();
    }

    @Test
    void isValid_shouldGetTrue_whenFilterValid() {
        assertTrue(booleanFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenValueNull() {
        booleanFilter.setValue(null);
        assertThrows(IllegalArgumentException.class,
                () -> booleanFilter.IsValid(max, min, regexp));
    }

}