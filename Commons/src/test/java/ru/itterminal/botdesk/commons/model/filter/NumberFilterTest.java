package ru.itterminal.botdesk.commons.model.filter;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;


@TestInstance(PER_CLASS)
class NumberFilterTest {

    private static final String IS_BETWEEN_EXCLUSION = "is_between_exclusion";
    private static final String GREATER_THAN = "greater_than";
    NumberFilter numberFilter;
    int max = Integer.MAX_VALUE;
    int min = 0;
    String regexp = "";

    @BeforeEach
    void setUp() {
        numberFilter = NumberFilter.builder()
                .valueOne(1)
                .valueTwo(2)
                .typeComparison(IS_BETWEEN_EXCLUSION)
                .build();
    }

    @Test
    void isValid_shouldGetTrue_whenFilterValid() {
        assertTrue(numberFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetTrue_whenFilter_GREATER_THAN_Valid() {
        numberFilter.setTypeComparison(GREATER_THAN);
        assertTrue(numberFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_GREATER_THAN_ValueOneNull() {
        numberFilter.setTypeComparison(GREATER_THAN);
        numberFilter.setValueOne(null);
        assertThrows(IllegalArgumentException.class,
                () -> numberFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_IS_BETWEEN_EXCLUSION_ValueOneNull() {
        numberFilter.setTypeComparison(IS_BETWEEN_EXCLUSION);
        numberFilter.setValueOne(null);
        assertThrows(IllegalArgumentException.class,
                () -> numberFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_IS_BETWEEN_EXCLUSION_ValueTwoNull() {
        numberFilter.setTypeComparison(IS_BETWEEN_EXCLUSION);
        numberFilter.setValueTwo(null);
        assertThrows(IllegalArgumentException.class,
                () -> numberFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_IS_BETWEEN_EXCLUSION_ValueTwoGreaterValueOne() {
        numberFilter.setTypeComparison(IS_BETWEEN_EXCLUSION);
        numberFilter.setValueTwo(0);
        assertThrows(IllegalArgumentException.class,
                () -> numberFilter.IsValid(max, min, regexp));
    }

}