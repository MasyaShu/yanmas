INSERT INTO account(out_id, deleted, version, id, name)
VALUES ('', 'false', '0', 'cdfa6483-0769-4628-ba32-efd338a716de', 'accountName1'),
       ('', 'false', '0', 'bcf98101-2a22-42bf-94cc-c900b50a0b69', 'accountName2');

INSERT INTO group_users(out_id, deleted, version, id, name, comment, account_id, is_deprecated, is_inner)
VALUES ('', 'false', '0', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7', 'groupName1', 'comment for group1 of users', 'cdfa6483-0769-4628-ba32-efd338a716de', 'false', 'true'),
       ('', 'false', '0', '63c1940d-e323-47af-8265-dbf8089727de', 'groupName3', 'comment for group3 of users', 'cdfa6483-0769-4628-ba32-efd338a716de', 'true', 'false'),
       ('', 'false', '0', '99f6b488-7687-4451-b8a1-9fbeb2a3efec', 'groupName2', 'comment for group2 of users', 'bcf98101-2a22-42bf-94cc-c900b50a0b69', 'false', 'false');

INSERT INTO users(out_id, deleted, version, id, email, first_name, second_name, password, phone, comment,
                 email_verification_token, email_verification_status, password_reset_token, is_archived,
                 account_id, own_group_id, role_id)
VALUES ('', 'false', '0', 'd592facb-e6ee-4801-8310-9c7708eb6e6c', 'm@m.ru', 'firstName1', 'secondName1',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '211-15-15', 'comment comment',
        '', 'false', '', 'false', 'cdfa6483-0769-4628-ba32-efd338a716de', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7',
        'ba99ce38-1611-4a81-adc9-3a779d58bbfe'),

       ('', 'false', '0', 'cdfa6483-0769-4628-ba32-efd338a716de', 'm1@m.ru', 'firstName2', 'secondName2',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '211-15-15', 'comment comment',
        '', 'false', '', 'false', 'cdfa6483-0769-4628-ba32-efd338a716de', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7',
        '933f20bf-9262-47bb-83d2-0ca55bbbd3fd'),

       ('', 'false', '0', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7', 'm2@m.ru', 'firstName3', 'secondName3',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '211-15-15', 'comment comment',
        '', 'false', '', 'false', 'cdfa6483-0769-4628-ba32-efd338a716de', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7',
        '933f20bf-9262-47bb-83d2-0ca55bbbd3fd'),

       ('', 'false', '0', 'e14d9ffd-0071-4c0e-99ed-932f007963f0', 'm3@m.ru', 'firstName4', 'secondName4',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '211-15-15', 'comment comment',
        '', 'false', '', 'false', 'cdfa6483-0769-4628-ba32-efd338a716de', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7',
        '933f20bf-9262-47bb-83d2-0ca55bbbd3fd'),

       ('', 'false', '0', '86840939-c488-448b-a473-cd9e1097dd32', 'm4@m.ru', 'firstName5', 'secondName5',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '211-15-15', 'comment comment',
        '', 'false', '', 'false', 'bcf98101-2a22-42bf-94cc-c900b50a0b69', '99f6b488-7687-4451-b8a1-9fbeb2a3efec',
        '933f20bf-9262-47bb-83d2-0ca55bbbd3fd');

