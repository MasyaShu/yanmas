DROP TABLE IF EXISTS test_table;
CREATE TABLE IF NOT EXISTS test_table
(
    id           uuid                     NOT NULL,
    display_name varchar(256)                      DEFAULT (NULL),
    out_id       varchar(128)                      DEFAULT (NULL),
    name         varchar(128)             NOT NULL,
    date         timestamp WITH time zone NOT NULL,
    description  varchar(255),
    deleted      bool                              DEFAULT 'false' NOT NULL,
    version      int                      NOT NULL DEFAULT (0),
    PRIMARY KEY (id)
);
CREATE UNIQUE INDEX test_table_name ON test_table (name);

INSERT INTO test_table(id, out_id, name, date, description, deleted, version)
VALUES ('d592facb-e6ee-4801-8310-9c7708eb6e6c', 'out_id_1', 'test_name_0', '2020-01-01T00:00:01.793+03:00[Europe/Kiev]',
        'general description', 'false', '0'),
       ('cdfa6483-0769-4628-ba32-efd338a716de', '', 'test_name_1', '2020-01-01T00:00:01.793+03:00[Europe/Kiev]',
        'general description', 'true', '0'),
       ('0223e51a-4bb2-44ee-bc8e-1f047a2145e7', '', 'test_name_2', '2020-03-01T00:00:01.793+03:00[Europe/Kiev]',
        'general description', 'true', '0'),
       ('e14d9ffd-0071-4c0e-99ed-932f007963f0', '', 'test_name_3', '2020-03-01T00:00:01.793+03:00[Europe/Kiev]',
        'general description', 'true', '0'),
       ('86840939-c488-448b-a473-cd9e1097dd32', '', 'test_name_4', '2020-03-01T00:00:01.793+03:00[Europe/Kiev]',
        'general description', 'true', '0'),
       ('b3805032-02db-4422-9c0e-4ddba1701811', '', 'test_name_5', '2020-03-01T00:00:01.793+03:00[Europe/Kiev]',
        'general description', 'true', '0'),
       ('27966676-1d47-488a-920a-06343f7e250f', '', 'test_name_6', '2020-03-01T00:00:01.793+03:00[Europe/Kiev]',
        'general description', 'true', '0'),
       ('95b10fdb-a1f2-498b-833c-caa2bed737dd', '', 'test_name_7', '2020-03-01T00:00:01.793+03:00[Europe/Kiev]',
        'general description', 'true', '0'),
       ('bf516877-43b9-4bff-85cc-8d46ab53767a', '', 'test_name_8', '2020-03-01T00:00:01.793+03:00[Europe/Kiev]',
        'general description', 'true', '0'),
       ('19e6458b-199a-4d02-9eea-38ff9da50d73', '', 'test_name_9', '2020-03-01T00:00:01.793+03:00[Europe/Kiev]',
        'general description', 'true', '0'),
       ('40e45625-e156-47d9-bb60-da45b17221bc', '', 'test_name_10', '2020-03-01T00:00:01.793+03:00[Europe/Kiev]',
        'general description', 'true', '0'),
       ('5e9aa820-8d77-4d8f-a8b6-6ccaf95b2cac', '', 'test_name_11', '2020-03-01T00:00:01.793+03:00[Europe/Kiev]',
        'general description', 'true', '0'),
       ('62d6a9eb-b902-4c5c-be58-57d9848d3a12', '', 'optimistic_locking_test',
        '2020-06-01T00:00:01.793+03:00[Europe/Kiev]',
        'not general description', 'false', '1'),
       ('e921073d-a204-4410-b9e6-8f9d46d07d5d', '', 'not_unique', '2020-09-01T00:00:01.793+03:00[Europe/Kiev]',
        'general description', 'true', '0');
