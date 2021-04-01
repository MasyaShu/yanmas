package ru.itterminal.yanmas.aau.model;

@SuppressWarnings("unused")
public enum Roles {
    ACCOUNT_OWNER(50),
    ADMIN(40),
    EXECUTOR(30),
    AUTHOR (20),
    OBSERVER (10);

    private final int weight;

    Roles(int weight) {
        this.weight = weight;
    }

    public int getWeight() {
        return weight;
    }
}
