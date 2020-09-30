CREATE TABLE IF NOT EXISTS account
(
    out_id   varchar(128),
    deleted  bool        NOT NULL DEFAULT 'false',
    version  int2        NOT NULL DEFAULT (0),
    id       uuid        NOT NULL,
    name     varchar(20) NOT NULL,
    language varchar(2),
    PRIMARY KEY (id)
);
CREATE TABLE IF NOT EXISTS group_users
(
    out_id  varchar(128),
    deleted bool         NOT NULL DEFAULT 'false',
    version int2         NOT NULL DEFAULT (0),
    id      uuid         NOT NULL,
    name    varchar(128) NOT NULL,
    comment text,
    PRIMARY KEY (id)
);
CREATE TABLE IF NOT EXISTS users
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
    language                  varchar(2),
    email_verification_token  varchar(128),
    email_verification_status bool         NOT NULL DEFAULT 'false',
    password_reset_token      varchar(128),
    is_archived               bool         NOT NULL DEFAULT 'false',
    account_id                uuid         NOT NULL,
    group_id                  uuid         NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (account_id) REFERENCES account (id),
    FOREIGN KEY (group_id) REFERENCES group_users (id)
);
CREATE TABLE IF NOT EXISTS role
(
    out_id  varchar(128),
    deleted bool        NOT NULL DEFAULT 'false',
    version int2        NOT NULL DEFAULT (0),
    id      uuid        NOT NULL,
    name    varchar(30) NOT NULL,
    PRIMARY KEY (id)
);
CREATE TABLE IF NOT EXISTS user_role
(
    id      uuid NOT NULL,
    user_id uuid NOT NULL,
    role_id uuid NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (user_id) REFERENCES users (id),
    FOREIGN KEY (role_id) REFERENCES role (id)
);
INSERT INTO role(out_id, deleted, version, id, name)
VALUES ('', 'false', '0', 'ba99ce38-1611-4a81-adc9-3a779d58bbfe', 'SUPER_ADMIN'),
       ('', 'false', '0', '607f04b1-f5f9-4f20-9c6f-501c32d773c0', 'ADMIN'),
       ('', 'false', '0', '933f20bf-9262-47bb-83d2-0ca55bbbd3fd', 'AUTHOR'),
       ('', 'false', '0', 'f7e579e6-0609-467a-91ff-454f42da3d58', 'EXECUTOR'),
       ('', 'false', '0', '586e087f-f5a0-4db8-af57-edead19db706', 'OBSERVER');