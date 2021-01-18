package ru.itterminal.botdesk.commons.model.filter;

import lombok.Builder;
import lombok.Data;

@SuppressWarnings("unused")
@Data
@Builder
public class BooleanFilter implements Filter {

    private Boolean value;

}
