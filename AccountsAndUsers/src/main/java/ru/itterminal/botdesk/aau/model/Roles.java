package ru.itterminal.botdesk.aau.model;

@SuppressWarnings("unused")
public enum Roles {
    ACCOUNT_OWNER(3),
    ADMIN(2),
    EXECUTOR(1),
    AUTHOR (0),
    OBSERVER (0);

    private final int weight;

    Roles(int weight) {
        this.weight = weight;
    }

    public int getWeight() {
        return weight;
    }
}
