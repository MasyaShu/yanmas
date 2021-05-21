package ru.itterminal.yanmas.tickets.service.business_handler.ticket;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.files.service.FileServiceImpl;
import ru.itterminal.yanmas.tickets.model.Ticket;

@Component
@RequiredArgsConstructor
public class SettingIntoFilesIdsOfTicketAfterCreateTicketBusinessHandler implements EntityBusinessHandler<Ticket> {

    private final FileServiceImpl fileService;

    @Override
    public void afterCreate(Ticket createdTicket, User currentUser) {
        if (createdTicket.getFiles() != null && !createdTicket.getFiles().isEmpty()) {
            for (File file : createdTicket.getFiles()) {
                file.setEntityId(createdTicket.getId());
                fileService.update(file, currentUser);
            }
        }
    }
}
