package ru.itterminal.botdesk.aau.model.dto;

public class UserDtoResponseWithoutPassword extends UserDto {

    @Override
    public void setPassword(String password) {
        super.setPassword(null);
    }
}
