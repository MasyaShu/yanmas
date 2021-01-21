package ru.itterminal.botdesk.commons.model.filter;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@SuppressWarnings("unused")
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BooleanFilter implements Filter {

    private Boolean value;

    @Override
    public boolean IsValid(int max, int min, String regexp) {
        if (value == null) {
            throw new IllegalArgumentException("Value must not be null");
        }
        return true;
    }

}
