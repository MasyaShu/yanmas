package ru.itterminal.botdesk.files.model;

import java.util.Objects;
import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.BaseEntity;

@Entity
@Table(name = "files")
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class File extends BaseEntity {

    @Column(name = "file_name", nullable = false, length = 260)
    private String fileName;

    @Column(nullable = false)
    private Integer size;

    @Column(name = "created_at", nullable = false)
    private Long createdAt;

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Column(name = "entity_id", nullable = false)
    private UUID entityId;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof File)) {
            return false;
        }
        File file = (File) o;
        return Objects.equals(fileName, file.fileName) &&
                Objects.equals(size, file.size) &&
                Objects.equals(createdAt, file.createdAt) &&
                Objects.equals(account, file.account) &&
                Objects.equals(entityId, file.entityId) &&
                Objects.equals(getId(), file.getId()) &&
                Objects.equals(getOutId(), file.getOutId()) &&
                Objects.equals(getVersion(), file.getVersion()) &&
                Objects.equals(getDeleted(), file.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(fileName, size, createdAt, account, entityId,
                            getId(), getOutId(), getVersion(), getDeleted()
        );
    }
}
