package ru.itterminal.botdesk.aau.model;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.BaseEntity;

import javax.persistence.*;

@Entity
@Table(name = "group_users")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Group extends BaseEntity {

    @Column(nullable = false, length = 128)
    private String name;

    @Column
    private String comment;

    @Column(name = "is_inner", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isInner;

    @Column(name = "is_deprecated", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isDeprecated;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Override
    public void generateDisplayName() {
        setDisplayName(name);
    }
}
