package ru.itterminal.botdesk.integration.across_modules;

import java.util.UUID;

public interface RequestsFromModuleAccountAndUsers {
    long countEntityOwnerByUser(UUID uuid);
}
