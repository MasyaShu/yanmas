package ru.itterminal.botdesk.commons.model.filter;

public interface Filter {
   boolean IsValid(int max, int min, String regexp);
}
