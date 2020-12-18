-- noinspection SpellCheckingInspectionForFile

INSERT INTO account(out_id, deleted, version, id, name)
VALUES (null, 'false', '0', 'cdfa6483-0769-4628-ba32-efd338a716de', 'accountName1'),
       (null, 'false', '0', 'bcf98101-2a22-42bf-94cc-c900b50a0b69', 'accountName2');

INSERT INTO group_users(out_id, deleted, version, id, name, comment, account_id, is_deprecated, is_inner)
VALUES (null, 'false', '0', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7', 'groupName1', 'comment for group1 of users',
        'cdfa6483-0769-4628-ba32-efd338a716de', 'false', 'true'),
       ('', 'false', '0', '63c1940d-e323-47af-8265-dbf8089727de', 'groupName3', '',
        'cdfa6483-0769-4628-ba32-efd338a716de', 'true', 'false'),
       (null, 'false', '0', '99f6b488-7687-4451-b8a1-9fbeb2a3efec', 'groupName2', null,
        'bcf98101-2a22-42bf-94cc-c900b50a0b69', 'false', 'false');

INSERT INTO users(out_id, deleted, version, id, email, name,  password, phone, comment,
                  email_verification_token, email_verification_status, password_reset_token, is_archived,
                  account_id, group_id, role_id)
VALUES (null, 'false', '0', 'd592facb-e6ee-4801-8310-9c7708eb6e6c', 'm@m.ru', 'Name1',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '211-15-15', 'comment comment',
        null, 'true', null, 'false', 'cdfa6483-0769-4628-ba32-efd338a716de', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7',
        'ba99ce38-1611-4a81-adc9-3a779d58bbfe'),

       (null, 'false', '0', 'cdfa6483-0769-4628-ba32-efd338a716de', 'm1@m.ru', 'Name2',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '211-15-15', 'comment comment',
        null, 'false', null, 'false', 'cdfa6483-0769-4628-ba32-efd338a716de', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7',
        '933f20bf-9262-47bb-83d2-0ca55bbbd3fd'),

       ('outId468431', 'false', '0', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7', 'm2@m.ru', null,
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', null, null,
        null, 'false', null, 'false', 'cdfa6483-0769-4628-ba32-efd338a716de', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7',
        '933f20bf-9262-47bb-83d2-0ca55bbbd3fd'),

       ('', 'false', '0', 'e14d9ffd-0071-4c0e-99ed-932f007963f0', 'm3@m.ru', '',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '', '',
        null, 'false', null, 'false', 'cdfa6483-0769-4628-ba32-efd338a716de', '0223e51a-4bb2-44ee-bc8e-1f047a2145e7',
        '933f20bf-9262-47bb-83d2-0ca55bbbd3fd'),

       (null, 'false', '0', '86840939-c488-448b-a473-cd9e1097dd32', 'm4@m.ru', 'Name5',
        '$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK', '211-15-15', 'comment comment',
        null, 'false', null, 'false', 'bcf98101-2a22-42bf-94cc-c900b50a0b69', '99f6b488-7687-4451-b8a1-9fbeb2a3efec',
        '933f20bf-9262-47bb-83d2-0ca55bbbd3fd');

INSERT INTO ticket_types(out_id, deleted, version, id, name, comment, is_predefined, account_id)
VALUES (null, 'false', '0', '7f66b241-f8ec-4912-8f58-a4ceef2dd4c9', 'ticketTypes1', 'comment1', true, 'cdfa6483-0769-4628-ba32-efd338a716de'),
       (null, 'false', '0', '17b13694-1907-4af9-8f5d-bfa444356e73', 'ticketTypes2', 'comment2', false, 'cdfa6483-0769-4628-ba32-efd338a716de'),
       (null, 'false', '0', 'dcf29ccb-26c7-4e38-9256-f45918a4c4a6', 'ticketTypes3', 'comment3', true, 'bcf98101-2a22-42bf-94cc-c900b50a0b69'),
       (null, 'false', '0', 'e251aad2-1a47-46ed-a405-0d8a6468627e', 'ticketTypes4', 'comment4', false, 'bcf98101-2a22-42bf-94cc-c900b50a0b69'),
       (null, 'false', '0', '6dc9c0de-2143-40ce-ac65-5be97e3019fc', 'ticketTypes5', '', false, 'bcf98101-2a22-42bf-94cc-c900b50a0b69');

INSERT INTO ticket_statuses(out_id, deleted, version, id, name, sort_index, is_started_predefined, is_finished_predefined, is_reopened_predefined, is_canceled_predefined, account_id)
VALUES (null, 'false', '0', '7f66b241-f8ec-4912-8f58-a4ceef2dd4c9', 'started', 10, true, false, false, false, 'cdfa6483-0769-4628-ba32-efd338a716de'),
       (null, 'false', '0', '17b13694-1907-4af9-8f5d-bfa444356e73', 'finished', 50, false, true, false, false, 'cdfa6483-0769-4628-ba32-efd338a716de'),
       (null, 'false', '0', 'dcf29ccb-26c7-4e38-9256-f45918a4c4a6', 'reopened', 20, false, false, true, false, 'bcf98101-2a22-42bf-94cc-c900b50a0b69'),
       (null, 'false', '0', 'e251aad2-1a47-46ed-a405-0d8a6468627e', 'canceled', 60, false, false, false, true,
        'bcf98101-2a22-42bf-94cc-c900b50a0b69'),
       (null, 'false', '0', '6dc9c0de-2143-40ce-ac65-5be97e3019fc', 'inWork', 40, false, false, false, false, 'bcf98101-2a22-42bf-94cc-c900b50a0b69');
INSERT INTO ticket_settings(out_id, deleted, version, id, account_id, group_id, author_id, ticket_type_id_for_new,
                            ticket_status_id_for_new, ticket_status_id_for_reopen, ticket_status_id_for_close,
                            ticket_status_id_for_cancel)
VALUES (null, 'false', '0', '9c8183ba-5d13-442f-a741-5b3134a3c140', 'cdfa6483-0769-4628-ba32-efd338a716de',
        '0223e51a-4bb2-44ee-bc8e-1f047a2145e7', 'd592facb-e6ee-4801-8310-9c7708eb6e6c',
        '7f66b241-f8ec-4912-8f58-a4ceef2dd4c9', '7f66b241-f8ec-4912-8f58-a4ceef2dd4c9',
        '17b13694-1907-4af9-8f5d-bfa444356e73', '17b13694-1907-4af9-8f5d-bfa444356e73',
        '7f66b241-f8ec-4912-8f58-a4ceef2dd4c9');

INSERT INTO ticket_settings_executors (out_id, deleted, version, id, ticket_settings_id, executor_id)
VALUES (null, 'false', '0', '1bab6fdd-4ef5-4222-a986-b6b2ae9e978d', '9c8183ba-5d13-442f-a741-5b3134a3c140',
        'd592facb-e6ee-4801-8310-9c7708eb6e6c'),
       (null, 'false', '0', '98137efe-4d0e-40db-ac78-7260861e289e', '9c8183ba-5d13-442f-a741-5b3134a3c140',
        'cdfa6483-0769-4628-ba32-efd338a716de');

INSERT INTO ticket_settings_observers (out_id, deleted, version, id, ticket_settings_id, observer_id)
VALUES (null, 'false', '0', '7b61d578-3eef-4a6e-99c9-6fa137f1bc0b', '9c8183ba-5d13-442f-a741-5b3134a3c140',
        '0223e51a-4bb2-44ee-bc8e-1f047a2145e7'),
       (null, 'false', '0', '968ee2a2-2af5-474b-b772-8b802aa58c6f', '9c8183ba-5d13-442f-a741-5b3134a3c140',
        'e14d9ffd-0071-4c0e-99ed-932f007963f0');


