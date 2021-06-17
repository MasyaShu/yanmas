package ru.itterminal.yanmas.aau.model;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.BaseEntity;

import javax.persistence.*;
import java.util.UUID;

@Entity
@Table(name = "property_values")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class PropertyValues extends BaseEntity {

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Column(name = "entity_id", nullable = false)
    private UUID entityId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "property_id", nullable = false)
    private Property property;

    @Column (nullable = false)
    private String value;

    @PrePersist
    protected void onCreate() {
        setDeleted(false);
    }

    @Override
    public void generateDisplayName() {
        setDisplayName(null);
    }
}
