package ru.itterminal.yanmas.aau.service.business_handler.impl;

import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;

import java.util.List;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.integration.email.SenderEmailViaSMTPServer;
import ru.itterminal.yanmas.security.jwt.JwtProvider;

@Component
@RequiredArgsConstructor
public class UserBusinessHandlerImpl implements EntityBusinessHandler<User> {

    private final BCryptPasswordEncoder encoder;
    private final JwtProvider jwtProvider;
    private final SenderEmailViaSMTPServer senderEmailViaSMTPServer;
    private final UserServiceImpl userService;
    protected final SpecificationsFactory specFactory;


    @Value("${email.VerificationEmailTokenForCreateAccount.subject}")
    private String emailVerificationTokenSubject;

    @Value("${email.VerificationEmailTokenForCreateAccount.textBody}")
    private String emailVerificationTokenTextBody;

    @Override
    public void beforeCreate(User entity, User currentUser) {
        entity.setPassword(encoder.encode(entity.getPassword()));
        setEmailVerificationToken(entity);
    }

    @Override
    public void afterCreate(User entity, User currentUser) {
        if (entity.getEmailVerificationToken() != null) {
            var email = senderEmailViaSMTPServer.createEmail(
                    entity.getEmail(),
                    emailVerificationTokenSubject,
                    emailVerificationTokenTextBody + " " + entity.getEmailVerificationToken()
            );
            senderEmailViaSMTPServer.sendEmail(email);
        }
    }

    @Override
    public void beforeUpdate(User entity, User currentUser) {
        var entityFromDatabase = userService.findByIdAndAccountId(entity.getId(), currentUser);
        if (entity.getPassword() == null || entity.getPassword().isEmpty()) {
            entity.setPassword(entityFromDatabase.getPassword());
        } else {
            entity.setPassword(encoder.encode(entity.getPassword()));
        }
        entity.setEmailVerificationStatus(entityFromDatabase.getEmailVerificationStatus());
        entity.setEmailVerificationToken(entityFromDatabase.getEmailVerificationToken());
        entity.setPasswordResetToken(entityFromDatabase.getPasswordResetToken());

    }

    @Override
    public Specification<User> beforeFindAllByFilter(Specification<User> specification, User currentUser) {
        if (currentUser.getRole().getWeight() <= Roles.AUTHOR.getWeight()
                || Boolean.FALSE.equals(currentUser.getGroup().getIsInner())) {
            var groupFilter = BaseEntityFilter.builder()
                    .typeComparison(EXIST_IN.toString())
                    .listOfIdEntities(List.of(currentUser.getGroup().getId()))
                    .build();
            var groupSpec = specFactory.makeSpecification(User.class, "group", groupFilter);
            return specification.and(groupSpec);
        }
        return specification;
    }

    private void setEmailVerificationToken(User entity) {
        if (entity.getRole().getName().equals(Roles.ACCOUNT_OWNER.toString())) {
            var emailVerificationToken = jwtProvider.createTokenWithUserId(entity.getId());
            entity.setEmailVerificationToken(emailVerificationToken);
            entity.setEmailVerificationStatus(false);
        } else {
            entity.setEmailVerificationStatus(true);
        }

    }
}
