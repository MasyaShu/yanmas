package ru.itterminal.botdesk.commons.model;

import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.MappedSuperclass;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Getter
@Setter
@MappedSuperclass
@NoArgsConstructor
@SuperBuilder
public class BaseEntityWithAccount extends BaseEntity {
    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;
}
