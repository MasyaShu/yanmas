package ru.itterminal.yanmas.commons.model.filter;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

@TestInstance(PER_CLASS)
class BaseEntityFilterTest {

    private static final String IS_EMPTY = "is_empty";
    private static final String EXIST_IN = "exist_in";
    private static final String IS_NOT_EMPTY = "is_not_empty";
    private static final String NOT_EXIST_IN = "not_exist_in";
    BaseEntityFilter baseEntityFilter;
    int max = Integer.MAX_VALUE;
    int min = 0;
    String regexp = "";
    List<UUID> uuidList = List.of(UUID.randomUUID(), UUID.randomUUID());

    @BeforeEach
    void setUp() {
        baseEntityFilter = BaseEntityFilter.builder()
                .listOfIdEntities(uuidList)
                .typeComparison(IS_EMPTY)
                .build();
    }

    @Test
    void isValid_shouldGetTrue_whenFilterValid() {
        assertTrue(baseEntityFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetTrue_whenFilter_EXIST_IN_ListUUIDNotNull() {
        baseEntityFilter.setTypeComparison(EXIST_IN);
        assertTrue(baseEntityFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetTrue_whenFilter_IS_EMPTY_ListUUIDNull() {
        baseEntityFilter.setTypeComparison(IS_EMPTY);
        baseEntityFilter.setListOfIdEntities(null);
        assertTrue(baseEntityFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetTrue_whenFilter_IS_NOT_EMPTY_ListUUIDEmpty() {
        baseEntityFilter.setTypeComparison(IS_NOT_EMPTY);
        baseEntityFilter.setListOfIdEntities(List.of());
        assertTrue(baseEntityFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_NOT_EXIST_IN_ListUUIDNull() {
        baseEntityFilter.setTypeComparison(NOT_EXIST_IN);
        baseEntityFilter.setListOfIdEntities(null);
        assertThrows(IllegalArgumentException.class,
                () -> baseEntityFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_EXIST_IN_ListUUIDNull() {
        baseEntityFilter.setTypeComparison(EXIST_IN);
        baseEntityFilter.setListOfIdEntities(null);
        assertThrows(IllegalArgumentException.class,
                () -> baseEntityFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_NOT_EXIST_IN_ListUUIDEmpty() {
        baseEntityFilter.setTypeComparison(NOT_EXIST_IN);
        baseEntityFilter.setListOfIdEntities(List.of());
        assertThrows(IllegalArgumentException.class,
                () -> baseEntityFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_EXIST_IN_ListUUIDEmpty() {
        baseEntityFilter.setTypeComparison(EXIST_IN);
        baseEntityFilter.setListOfIdEntities(List.of());
        assertThrows(IllegalArgumentException.class,
                () -> baseEntityFilter.IsValid(max, min, regexp));
    }


}