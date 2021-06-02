create TABLE IF NOT EXISTS files
(
    out_id       varchar(128),
    display_name varchar(256)          DEFAULT (NULL),
    deleted      bool         NOT NULL DEFAULT 'false',
    version      int4         NOT NULL DEFAULT (0),
    id           uuid         NOT NULL,
    file_name    varchar(256) NOT NULL,
    author_id    uuid         NOT NULL,
    size         int,
    created_at   bigint       NOT NULL,
    account_id   uuid         NOT NULL,
    entity_id    uuid                  DEFAULT (NULL),
    is_uploaded  bool         NOT NULL DEFAULT 'false',
    PRIMARY KEY (id),
    FOREIGN KEY (account_id) REFERENCES accounts (id)
);
