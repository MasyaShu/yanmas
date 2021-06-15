package ru.itterminal.yanmas.commons.model;

import lombok.*;
import lombok.experimental.SuperBuilder;

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
