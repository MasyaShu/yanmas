package ru.itterminal.botdesk.aau.model;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.BaseEntity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.PrePersist;
import javax.persistence.Table;

@Entity
@Table(name = "accounts")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Account extends BaseEntity {

    @Column(nullable = false, length = 128)
    private String name;

    @Override
    public void generateDisplayName() {
        setDisplayName(name);
    }

    @PrePersist
    protected void onCreate() {
        setDeleted(false);
    }
}
