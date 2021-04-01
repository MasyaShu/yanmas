package ru.itterminal.yanmas.files.repository;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.springframework.core.io.FileSystemResource;
import org.springframework.stereotype.Repository;

@Repository
public class FileSystemRepository {

    public void save(byte[] content, Path location) throws IOException {
        Files.createFile(location);
        Files.write(location, content);
    }

    public FileSystemResource findInFileSystem(Path location) {
        return new FileSystemResource(location);
    }
}
