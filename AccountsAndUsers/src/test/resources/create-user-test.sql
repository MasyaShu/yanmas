INSERT INTO account(out_id, deleted, version, id, name, language)
VALUES ('', 'false', '0', 'cdfa6483-0769-4628-ba32-efd338a716de', 'accountName1', 'ru'),
       ('', 'false', '0', 'bcf98101-2a22-42bf-94cc-c900b50a0b69', 'accountName2', 'en');

INSERT INTO group_users(out_id, deleted, version, id, name, comment)
VALUES ('', 'false', '0', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7', 'groupName1', 'comment for group1 of users'),
       ('', 'false', '0', '99f6b488-7687-4451-b8a1-9fbeb2a3efec', 'groupName2', 'comment for group2 of users');

INSERT INTO users(out_id, deleted, version, id, email, first_name, second_name, password, phone, comment, language,
                 email_verification_token, email_verification_status, password_reset_token, is_archived,
                 account_id, group_id)
VALUES ('', 'false', '0', 'd592facb-e6ee-4801-8310-9c7708eb6e6c', 'm@m.ru', 'firstName1', 'secondName1',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '211-15-15', 'comment comment', 'ru',
        '', 'false', '', 'false', 'cdfa6483-0769-4628-ba32-efd338a716de', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7'),

       ('', 'false', '0', 'cdfa6483-0769-4628-ba32-efd338a716de', 'm1@m.ru', 'firstName2', 'secondName2',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '211-15-15', 'comment comment', 'ru',
        '', 'false', '', 'false', 'cdfa6483-0769-4628-ba32-efd338a716de', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7'),

       ('', 'false', '0', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7', 'm2@m.ru', 'firstName3', 'secondName3',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '211-15-15', 'comment comment', 'ru',
        '', 'false', '', 'false', 'cdfa6483-0769-4628-ba32-efd338a716de', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7'),

       ('', 'false', '0', 'e14d9ffd-0071-4c0e-99ed-932f007963f0', 'm3@m.ru', 'firstName4', 'secondName4',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '211-15-15', 'comment comment', 'ru',
        '', 'false', '', 'false', 'cdfa6483-0769-4628-ba32-efd338a716de', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7'),

       ('', 'false', '0', '86840939-c488-448b-a473-cd9e1097dd32', 'm4@m.ru', 'firstName5', 'secondName5',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '211-15-15', 'comment comment', 'ru',
        '', 'false', '', 'false', 'bcf98101-2a22-42bf-94cc-c900b50a0b69', '99f6b488-7687-4451-b8a1-9fbeb2a3efec');

INSERT INTO user_role(id, user_id, role_id)
VALUES ('b32cde61-5ea5-4895-bfc3-726d5eb6fa6f', 'd592facb-e6ee-4801-8310-9c7708eb6e6c',
        'ba99ce38-1611-4a81-adc9-3a779d58bbfe'),
       ('264455ff-73bf-4fc6-9ab9-bf127e527a08', 'cdfa6483-0769-4628-ba32-efd338a716de',
        '933f20bf-9262-47bb-83d2-0ca55bbbd3fd'),
       ('dbb8296b-a5bd-48b1-b730-64e8516013af', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7',
        '933f20bf-9262-47bb-83d2-0ca55bbbd3fd'),
       ('0fc5c7d4-8841-4db3-aa49-aee3c8305b62', 'e14d9ffd-0071-4c0e-99ed-932f007963f0',
        '933f20bf-9262-47bb-83d2-0ca55bbbd3fd'),
       ('4a1e233f-a947-4774-8e32-75ac167846e2', '86840939-c488-448b-a473-cd9e1097dd32',
        '933f20bf-9262-47bb-83d2-0ca55bbbd3fd');