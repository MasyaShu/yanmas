-- START MODULE **ACCOUNT AND USERS**
create TABLE IF NOT EXISTS account
(
    out_id  varchar(128),
    deleted bool        NOT NULL DEFAULT 'false',
    version int2        NOT NULL DEFAULT (0),
    id      uuid        NOT NULL,
    name    varchar(20) NOT NULL,
    PRIMARY KEY (id)
);
create TABLE IF NOT EXISTS group_users
(
    out_id        varchar(128),
    deleted       bool         NOT NULL DEFAULT 'false',
    version       int2         NOT NULL DEFAULT (0),
    id            uuid         NOT NULL,
    name          varchar(128) NOT NULL,
    comment       text,
    is_deprecated bool         NOT NULL DEFAULT 'false',
    is_inner      bool         NOT NULL DEFAULT 'false',
    account_id    uuid         NOT NULL,
    PRIMARY KEY (id)
);
create TABLE IF NOT EXISTS role
(
    out_id  varchar(128),
    deleted bool        NOT NULL DEFAULT 'false',
    version int2        NOT NULL DEFAULT (0),
    id      uuid        NOT NULL,
    name    varchar(30) NOT NULL,
    weight  int         NOT NULL,
    PRIMARY KEY (id)
);
create TABLE IF NOT EXISTS users
(
    out_id                    varchar(128),
    deleted                   bool         NOT NULL DEFAULT 'false',
    version                   int2         NOT NULL DEFAULT (0),
    id                        uuid         NOT NULL,
    email                     varchar(128) NOT NULL UNIQUE,
    first_name                varchar(20),
    second_name               varchar(30),
    password                  VARCHAR(128) NOT NULL,
    phone                     varchar(30),
    comment                   text,
    email_verification_token  varchar(256),
    email_verification_status bool         NOT NULL DEFAULT 'false',
    password_reset_token      varchar(256),
    is_archived               bool         NOT NULL DEFAULT 'false',
    account_id                uuid         NOT NULL,
    role_id                   uuid         NOT NULL,
    own_group_id              uuid         NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (role_id) REFERENCES role (id),
    FOREIGN KEY (account_id) REFERENCES account (id),
    FOREIGN KEY (own_group_id) REFERENCES group_users (id)
);
create TABLE IF NOT EXISTS ticket_types
(
    out_id  varchar(128),
    deleted bool         NOT NULL DEFAULT 'false',
    version int2         NOT NULL DEFAULT (0),
    id      uuid         NOT NULL,
    name    varchar(128) NOT NULL,
    comment text,
    is_predefined bool  NOT NULL DEFAULT 'false',
    account_id  uuid    NOT NULL,
    PRIMARY KEY (id)
);
insert into role(out_id, deleted, version, id, name, weight)
values (null, 'false', '0', 'ba99ce38-1611-4a81-adc9-3a779d58bbfe', 'ACCOUNT_OWNER', '3'),
       (null, 'false', '0', '607f04b1-f5f9-4f20-9c6f-501c32d773c0', 'ADMIN', '2'),
       (null, 'false', '0', 'f7e579e6-0609-467a-91ff-454f42da3d58', 'EXECUTOR', '1'),
       (null, 'false', '0', '933f20bf-9262-47bb-83d2-0ca55bbbd3fd', 'AUTHOR', '0'),
       (null, 'false', '0', '586e087f-f5a0-4db8-af57-edead19db706', 'OBSERVER', '0');
-- START MODULE **FILES**
create TABLE IF NOT EXISTS files
(
    out_id      varchar(128),
    deleted     bool         NOT NULL DEFAULT 'false',
    version     int2         NOT NULL DEFAULT (0),
    id          uuid         NOT NULL,
    file_name   varchar(256) NOT NULL,
    size        int          NOT NULL,
    created_at  bigint       NOT NULL,
    account_id  uuid         NOT NULL,
    entity_id   uuid         NOT NULL,
    is_uploaded bool         NOT NULL DEFAULT 'false',
    PRIMARY KEY (id),
    FOREIGN KEY (account_id) REFERENCES account (id)
);