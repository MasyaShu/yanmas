package ru.itterminal.yanmas.aau.model;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.BaseEntity;

import javax.persistence.*;

@Entity
@Table(name = "property_groups")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class PropertyGroup extends BaseEntity {

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Column(nullable = false, length = 256)
    private String name;

    @Column (name = "order_view")
    private Integer orderView;

    @Column
    private String description;

    @PrePersist
    protected void onCreate() {
        setDeleted(false);
    }

    @Override
    public void generateDisplayName() {
        setDisplayName(name);
    }
}
