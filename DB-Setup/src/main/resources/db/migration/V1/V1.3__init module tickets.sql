create TABLE IF NOT EXISTS ticket_types
(
    out_id        varchar(128),
    display_name  varchar(256)          DEFAULT (NULL),
    deleted       bool         NOT NULL DEFAULT 'false',
    version       int4         NOT NULL DEFAULT (0),
    id            uuid         NOT NULL,
    name          varchar(128) NOT NULL,
    comment       text,
    is_predefined bool         NOT NULL DEFAULT 'false',
    account_id    uuid         NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (account_id) REFERENCES accounts (id)
);

create TABLE IF NOT EXISTS ticket_statuses
(
    out_id                 varchar(128),
    display_name           varchar(256)          DEFAULT (NULL),
    deleted                bool         NOT NULL DEFAULT 'false',
    version                int4         NOT NULL DEFAULT (0),
    id                     uuid         NOT NULL,
    name                   varchar(128) NOT NULL,
    sort_index             int2         NOT NULL,
    is_started_predefined  bool         NOT NULL DEFAULT 'false',
    is_finished_predefined bool         NOT NULL DEFAULT 'false',
    is_reopened_predefined bool         NOT NULL DEFAULT 'false',
    is_canceled_predefined bool         NOT NULL DEFAULT 'false',
    account_id             uuid         NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (account_id) REFERENCES accounts (id)
);

create TABLE IF NOT EXISTS ticket_templates
(
    out_id                     varchar(128),
    display_name               varchar(256)     DEFAULT (NULL),
    deleted                    bool    NOT NULL DEFAULT 'false',
    version                    int4    NOT NULL DEFAULT (0),
    id                         uuid    NOT NULL,
    subject                    varchar(256),
    description                text,
    date_next_run              bigint,
    date_start                 bigint,
    date_end                   bigint,
    zone_id                    varchar NOT NULL,
    expression_schedule        varchar NOT NULL,
    is_only_one_ticket_in_work bool    NOT NULL DEFAULT 'false',
    is_active                  bool    NOT NULL DEFAULT 'true',
    account_id                 uuid    NOT NULL,
    author_id                  uuid    NOT NULL,
    ticket_type_id             uuid    NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (account_id) REFERENCES accounts (id),
    FOREIGN KEY (author_id) REFERENCES users (id),
    FOREIGN KEY (ticket_type_id) REFERENCES ticket_types (id)
);

create TABLE IF NOT EXISTS ticket_settings
(
    out_id                      varchar(128),
    display_name                varchar(256)  DEFAULT (NULL),
    deleted                     bool NOT NULL DEFAULT 'false',
    version                     int4 NOT NULL DEFAULT (0),
    id                          uuid NOT NULL,
    account_id                  uuid NOT NULL,
    author_id                   uuid,
    group_id                    uuid,
    ticket_type_id_for_new      uuid,
    ticket_status_id_for_new    uuid,
    ticket_status_id_for_reopen uuid,
    ticket_status_id_for_close  uuid,
    ticket_status_id_for_cancel uuid,
    PRIMARY KEY (id),
    FOREIGN KEY (account_id) REFERENCES accounts (id),
    FOREIGN KEY (author_id) REFERENCES users (id),
    FOREIGN KEY (group_id) REFERENCES group_users (id),
    FOREIGN KEY (ticket_type_id_for_new) REFERENCES ticket_types (id),
    FOREIGN KEY (ticket_status_id_for_new) REFERENCES ticket_statuses (id),
    FOREIGN KEY (ticket_status_id_for_reopen) REFERENCES ticket_statuses (id),
    FOREIGN KEY (ticket_status_id_for_close) REFERENCES ticket_statuses (id),
    FOREIGN KEY (ticket_status_id_for_cancel) REFERENCES ticket_statuses (id)
);

CREATE UNIQUE INDEX unique_fields_ticket_settings ON ticket_settings (account_id, group_id, author_id);

create TABLE IF NOT EXISTS ticket_settings_executors
(
    ticket_settings_id uuid NOT NULL,
    executor_id        uuid NOT NULL,
    FOREIGN KEY (ticket_settings_id) REFERENCES ticket_settings (id),
    FOREIGN KEY (executor_id) REFERENCES users (id)
);

create TABLE IF NOT EXISTS ticket_settings_observers
(
    ticket_settings_id uuid NOT NULL,
    observer_id        uuid NOT NULL,
    FOREIGN KEY (ticket_settings_id) REFERENCES ticket_settings (id),
    FOREIGN KEY (observer_id) REFERENCES users (id)
);

create TABLE IF NOT EXISTS ticket_counters
(
    out_id         varchar(128),
    display_name   varchar(256)    DEFAULT (NULL),
    deleted        bool   NOT NULL DEFAULT 'false',
    version        int4   NOT NULL DEFAULT (0),
    id             uuid   NOT NULL,
    current_number bigint NOT NULL,
    PRIMARY KEY (id)
);