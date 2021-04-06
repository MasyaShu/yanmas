package ru.itterminal.yanmas.aau.model;

import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.PrePersist;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.BaseEntity;

@Entity
@Table(name = "who_watched_entity")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@EqualsAndHashCode
public class WhoWatchedEntity {
    @Id
    private UUID id;

    @Column(name = "account_id",  nullable = false)
    private UUID accountId;

    @Column(name = "entity_id", nullable = false)
    private UUID entityId;

    @Column(name = "user_id",  nullable = false)
    private UUID userId;
}
