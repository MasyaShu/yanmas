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
class ListOfBaseEntityFilterTest {

    private static final String IS_EQUAL_TO = "is_equal_to";
    private static final String IS_NOT_EQUAL_TO = "is_not_equal_to";
    private static final String IS_NOT_EMPTY = "is_not_empty";
    private static final String IS_EMPTY = "is_empty";
    ListOfBaseEntityFilter baseEntityFilter;
    int max = Integer.MAX_VALUE;
    int min = 0;
    String regexp = "";
    List<UUID> uuidList = List.of(UUID.randomUUID(), UUID.randomUUID());

    @BeforeEach
    void setUp() {
        baseEntityFilter = ListOfBaseEntityFilter.builder()
                .listOfIdEntities(uuidList)
                .typeComparison("is_empty")
                .build();
    }

    @Test
    void isValid_shouldGetTrue_whenFilterValid() {
        assertTrue(baseEntityFilter.IsValid(max, min, regexp));
    }


    @Test
    void isValid_shouldGetTrue_whenFilter_IS_EQUAL_TO_ListUUIDNotNull() {
        baseEntityFilter.setTypeComparison(IS_EQUAL_TO);
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
    void isValid_shouldGetIllegalArgumentException_whenFilter_IS_NOT_EQUAL_TO_ListUUIDNull() {
        baseEntityFilter.setTypeComparison(IS_NOT_EQUAL_TO);
        baseEntityFilter.setListOfIdEntities(null);
        assertThrows(IllegalArgumentException.class,
                () -> baseEntityFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_IS_EQUAL_TO_ListUUIDNull() {
        baseEntityFilter.setTypeComparison(IS_EQUAL_TO);
        baseEntityFilter.setListOfIdEntities(null);
        assertThrows(IllegalArgumentException.class,
                () -> baseEntityFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_IS_NOT_EQUAL_TO_ListUUIDEmpty() {
        baseEntityFilter.setTypeComparison(IS_NOT_EQUAL_TO);
        baseEntityFilter.setListOfIdEntities(List.of());
        assertThrows(IllegalArgumentException.class,
                () -> baseEntityFilter.IsValid(max, min, regexp));
    }

    @Test
    void isValid_shouldGetIllegalArgumentException_whenFilter_IS_EQUAL_TO_ListUUIDEmpty() {
        baseEntityFilter.setTypeComparison(IS_EQUAL_TO);
        baseEntityFilter.setListOfIdEntities(List.of());
        assertThrows(IllegalArgumentException.class,
                () -> baseEntityFilter.IsValid(max, min, regexp));
    }

}