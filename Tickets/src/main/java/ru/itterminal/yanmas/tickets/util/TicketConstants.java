package ru.itterminal.yanmas.tickets.util;

public class TicketConstants {
    public static final String COMPARISON_PATTERN = "^<=$|^<$|^=>$|^>$|^=$";
    public static final String INVALID_COMPARISON =
            "Comparison expression consists only <= < >= > =";

    private TicketConstants() {
    }
}
