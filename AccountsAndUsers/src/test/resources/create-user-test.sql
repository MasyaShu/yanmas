DROP TABLE IF EXISTS user;
CREATE TABLE IF NOT EXISTS user
(
    id          uuid                     NOT NULL,
    email       varchar(128)             NOT NULL UNIQUE,
    PRIMARY KEY (id)
);

INSERT INTO user(id, email)
VALUES ('d592facb-e6ee-4801-8310-9c7708eb6e6c', 'm@m.ru'),
       ('cdfa6483-0769-4628-ba32-efd338a716de', 'm1@m.ru'),
       ('0223e51a-4bb2-44ee-bc8e-1f047a2145e7', 'm2@m.ru'),
       ('e14d9ffd-0071-4c0e-99ed-932f007963f0', 'm3@m.ru'),
       ('86840939-c488-448b-a473-cd9e1097dd32', 'm4@m.ru');
