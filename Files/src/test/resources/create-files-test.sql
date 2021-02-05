-- noinspection SpellCheckingInspectionForFile

INSERT INTO accounts(out_id, deleted, version, id, name)
VALUES (null, 'false', '0', 'cdfa6483-0769-4628-ba32-efd338a716de', 'accountName1'),
       (null, 'false', '0', 'bcf98101-2a22-42bf-94cc-c900b50a0b69', 'accountName2');

INSERT INTO files(out_id, deleted, version, id, file_name, size, created_at, account_id, entity_id, is_uploaded, author_id)
VALUES (null, 'false', '0', 'd286a51a-a834-48ed-a39b-5ae3d0640001', 'file_name_1_accountName1.txt', 1000, 1606967833,
        'cdfa6483-0769-4628-ba32-efd338a716de', 'c2760b6b-33a4-4ee0-bfad-f81128c08754', false, 'ce493fd4-20ca-4d93-85ce-9da4254f311b'),
       (null, 'false', '0', 'de0b09e3-6a9d-47f6-b850-e569505dbf0a', 'file_name_2_accountName1.txt', 1000, 1606967834,
        'cdfa6483-0769-4628-ba32-efd338a716de', 'c2760b6b-33a4-4ee0-bfad-f81128c08754', false, 'ce493fd4-20ca-4d93-85ce-9da4254f311b'),
       (null, 'false', '0', '255bddff-4f78-42dc-8ab6-2197245a23bf', 'file_name_1_accountName2.txt', 1000, 1606967835,
        'cdfa6483-0769-4628-ba32-efd338a716de', '19ee6b46-f11f-429b-9ae1-336af900cad3', false, '3e440d4b-7542-419d-8da9-a7cc7bb29cd7');