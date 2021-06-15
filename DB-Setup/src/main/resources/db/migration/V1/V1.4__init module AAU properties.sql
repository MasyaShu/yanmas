create TABLE IF NOT EXISTS property_groups
(
    out_id       varchar(128),
    display_name varchar(256)          DEFAULT (NULL),
    deleted      bool         NOT NULL DEFAULT 'false',
    version      int4         NOT NULL DEFAULT (0),
    id           uuid         NOT NULL,
    account_id   uuid         NOT NULL,
    name         varchar(256) NOT NULL,
    order_view   int2         NOT NULL DEFAULT (0),
    description  text,
    PRIMARY KEY (id),
    FOREIGN KEY (account_id) REFERENCES accounts (id)
);
create TABLE IF NOT EXISTS properties
(
    out_id        varchar(128),
    display_name  varchar(256)          DEFAULT (NULL),
    deleted       bool         NOT NULL DEFAULT 'false',
    version       int4         NOT NULL DEFAULT (0),
    id            uuid         NOT NULL,
    account_id    uuid         NOT NULL,
    name          varchar(256) NOT NULL,
    type_property varchar(7)   NOT NULL,
    type_entity   varchar(128) NOT NULL,
    order_view    int2         NOT NULL DEFAULT (0),
    description   text,
    is_system     bool         NOT NULL DEFAULT 'false',
    PRIMARY KEY (id),
    FOREIGN KEY (account_id) REFERENCES accounts (id)
);
create TABLE IF NOT EXISTS property_values
(
    out_id       varchar(128),
    display_name varchar(256)  DEFAULT (NULL),
    deleted      bool NOT NULL DEFAULT 'false',
    version      int4 NOT NULL DEFAULT (0),
    account_id   uuid NOT NULL,
    id           uuid NOT NULL,
    entity_id    uuid NOT NULL,
    property_id  uuid NOT NULL,
    value        text NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (property_id) REFERENCES properties (id),
    FOREIGN KEY (account_id) REFERENCES accounts (id)
);
create TABLE IF NOT EXISTS initial_properties_for_new_account
(
    name                        varchar(256) NOT NULL,
    type_property               varchar(7)   NOT NULL,
    type_entity                 varchar(128) NOT NULL,
    order_view                  int2         NOT NULL DEFAULT (0),
    description                 text,
    property_groups_name        varchar(256),
    property_groups_description text,
    property_groups_order_view  int2
);