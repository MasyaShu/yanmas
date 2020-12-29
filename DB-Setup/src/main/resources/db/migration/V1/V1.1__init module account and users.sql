create TABLE IF NOT EXISTS accounts
(
    out_id       varchar(128),
    display_name varchar(256)         DEFAULT (NULL),
    deleted      bool        NOT NULL DEFAULT 'false',
    version      int4        NOT NULL DEFAULT (0),
    id           uuid        NOT NULL,
    name         varchar(20) NOT NULL,
    PRIMARY KEY (id)
);
create TABLE IF NOT EXISTS group_users
(
    out_id        varchar(128),
    display_name  varchar(256)          DEFAULT (NULL),
    deleted       bool         NOT NULL DEFAULT 'false',
    version       int4         NOT NULL DEFAULT (0),
    id            uuid         NOT NULL,
    name          varchar(128) NOT NULL,
    comment       text,
    is_deprecated bool         NOT NULL DEFAULT 'false',
    is_inner      bool         NOT NULL DEFAULT 'false',
    account_id    uuid         NOT NULL,
    PRIMARY KEY (id)
);
create TABLE IF NOT EXISTS roles
(
    out_id       varchar(128),
    display_name varchar(256)         DEFAULT (NULL),
    deleted      bool        NOT NULL DEFAULT 'false',
    version      int4        NOT NULL DEFAULT (0),
    id           uuid        NOT NULL,
    name         varchar(30) NOT NULL,
    weight       int         NOT NULL,
    PRIMARY KEY (id)
);
create TABLE IF NOT EXISTS users
(
    out_id                    varchar(128),
    display_name              varchar(256)          DEFAULT (NULL),
    deleted                   bool         NOT NULL DEFAULT 'false',
    version                   int4         NOT NULL DEFAULT (0),
    id                        uuid         NOT NULL,
    email                     varchar(128) NOT NULL UNIQUE,
    name                      varchar(128),
    password                  VARCHAR(128) NOT NULL,
    phone                     varchar(30),
    comment                   text,
    email_verification_token  varchar(256),
    email_verification_status bool         NOT NULL DEFAULT 'false',
    password_reset_token      varchar(256),
    is_archived               bool         NOT NULL DEFAULT 'false',
    account_id                uuid         NOT NULL,
    role_id                   uuid         NOT NULL,
    group_id                  uuid         NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (role_id) REFERENCES roles (id),
    FOREIGN KEY (account_id) REFERENCES accounts (id),
    FOREIGN KEY (group_id) REFERENCES group_users (id)
);