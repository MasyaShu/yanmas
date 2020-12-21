package ru.itterminal.botdesk.files.model;

import java.util.Objects;
import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.BaseEntity;

@Entity
@Table(name = "files")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class File extends BaseEntity {

    @Column(name = "file_name", nullable = false, length = 260)
    private String fileName;

    @Column(nullable = false)
    private Integer size;

    @Column(name = "created_at", nullable = false)
    private Long createdAt;

    @Column(name = "entity_id", nullable = false)
    private UUID entityId;

    @Column(name = "is_uploaded", nullable = false)
    private Boolean isUploaded;

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Override
    public void generateDisplayName() {
        setDisplayName(fileName);
    }
}
