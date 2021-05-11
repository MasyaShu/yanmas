package ru.itterminal.yanmas.integration.across_modules;

import java.util.UUID;

public interface CompletedVerificationAccount {
    void actionAfterCompletedVerificationAccount(UUID uuid, UUID idCurrentUser);
}
