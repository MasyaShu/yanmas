package ru.itterminal.botdesk.aau.model;

import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

@Entity
@Table(name="role")
@Setter
@Getter
public class Role {

    @Id
    private UUID id;

    @Column (nullable = false)
    private String name;

}
