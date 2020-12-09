-- noinspection SpellCheckingInspectionForFile

INSERT INTO account(out_id, deleted, version, id, name)
VALUES (null, 'false', '0', 'cdfa6483-0769-4628-ba32-efd338a716de', 'accountName1'),
       (null, 'false', '0', 'bcf98101-2a22-42bf-94cc-c900b50a0b69', 'accountName2');

INSERT INTO ticket_types(out_id, deleted, version, id, name, comment, is_predefined, account_id)
VALUES (null, 'false', '0', '7f66b241-f8ec-4912-8f58-a4ceef2dd4c9', 'ticketTypes1', 'comment1', true, 'cdfa6483-0769-4628-ba32-efd338a716de'),
       (null, 'false', '0', '17b13694-1907-4af9-8f5d-bfa444356e73', 'ticketTypes2', 'comment2', false, 'cdfa6483-0769-4628-ba32-efd338a716de'),
       (null, 'false', '0', 'dcf29ccb-26c7-4e38-9256-f45918a4c4a6', 'ticketTypes3', 'comment3', true, 'bcf98101-2a22-42bf-94cc-c900b50a0b69'),
       (null, 'false', '0', 'e251aad2-1a47-46ed-a405-0d8a6468627e', 'ticketTypes4', 'comment4', false, 'bcf98101-2a22-42bf-94cc-c900b50a0b69'),
       (null, 'false', '0', '6dc9c0de-2143-40ce-ac65-5be97e3019fc', 'ticketTypes5', '', false, 'bcf98101-2a22-42bf-94cc-c900b50a0b69');



