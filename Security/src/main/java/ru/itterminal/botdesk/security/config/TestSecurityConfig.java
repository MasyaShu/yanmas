package ru.itterminal.botdesk.security.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Profile;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.web.cors.CorsConfigurationSource;
import ru.itterminal.botdesk.security.jwt.JwtUser;

import java.util.List;
import java.util.UUID;

@EnableGlobalMethodSecurity(securedEnabled = true, prePostEnabled = true)
@EnableWebSecurity
@Profile("Test")
public class TestSecurityConfig extends WebSecurityConfigurerAdapter {

    public static final String ACCOUNT_1_ID = "cdfa6483-0769-4628-ba32-efd338a716de";
    public static final String ACCOUNT_2_ID = "bcf98101-2a22-42bf-94cc-c900b50a0b69";
    public static final String INNER_GROUP_ID = "0223e51a-4bb2-44ee-bc8e-1f047a2145e7";
    public static final String NOT_INNER_GROUP_ID = "63c1940d-e323-47af-8265-dbf8089727de";
    public static final String ROLE_ADMIN = "ADMIN";
    public static final String ROLE_AUTHOR = "AUTHOR";
    public static final String ROLE_EXECUTOR = "EXECUTOR";
    public static final String ROLE_OBSERVER = "OBSERVER";
    public static final String ROLE_ACCOUNT_OWNER = "ACCOUNT_OWNER";
    public static final String EMAIL_1 = "Mackenzie.lang@gmail.com";
    public static final int WEIGHT_ROLE_50 = 50;
    public static final int WEIGHT_ROLE_40 = 40;
    public static final int WEIGHT_ROLE_30 = 30;
    public static final int WEIGHT_ROLE_20 = 20;
    public static final int WEIGHT_ROLE_10 = 10;
    public static final String OWNER_ACCOUNT_1_IS_INNER_GROUP_ID = "55827ea4-f7bf-411e-897e-bc29da2cca65";
    public static final String OWNER_ACCOUNT_2_IS_INNER_GROUP_ID = "4c91bfe2-30bf-4ceb-b6a0-ca94fb04c139";
    public static final String ADMIN_ACCOUNT_1_IS_INNER_GROUP_ID = "d0d6f6b6-0443-4394-911c-c843e32fd454";
    public static final String ADMIN_ACCOUNT_2_IS_INNER_GROUP_ID = "be3bb7b9-3be8-4054-8f6a-3c9b8c1466d9";
    public static final String ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP_ID = "d17e76db-6abd-4efe-a423-097c89d430ca";
    public static final String ADMIN_ACCOUNT_2_IS_NOT_INNER_GROUP_ID = "754821aa-00c6-424b-a433-11a429d5de23";
    public static final String EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP_ID = "e9178ed9-e910-4121-b206-b2530df2731e";
    public static final String EXECUTOR_ACCOUNT_1_IS_INNER_GROUP_ID = "f447e752-ed02-4acc-9cb1-3ae2a53f10f6";
    public static final String EXECUTOR_ACCOUNT_2_IS_NOT_INNER_GROUP_ID = "2c4488f0-ed14-41ae-932f-3382e03c78a1";
    public static final String EXECUTOR_ACCOUNT_2_IS_INNER_GROUP_ID = "6c3fb50a-7867-4414-bdbd-f1780b63e883";
    public static final String AUTHOR_ACCOUNT_1_IS_NOT_INNER_GROUP_ID = "a4a66812-c5d2-4ebb-8b50-2387fd70f049";
    public static final String AUTHOR_ACCOUNT_1_IS_INNER_GROUP_ID = "4a45a88d-8500-4157-8460-4b4f5a42b731";
    public static final String AUTHOR_ACCOUNT_2_IS_NOT_INNER_GROUP_ID = "4a610be0-4d5d-4815-89bc-92977a141a1c";
    public static final String AUTHOR_ACCOUNT_2_IS_INNER_GROUP_ID = "cbb94e02-9f77-4bc1-84c5-d44a44718368";
    public static final String OBSERVER_ACCOUNT_1_IS_NOT_INNER_GROUP_ID = "8dc4c4d0-9bbd-4c9a-bcc1-4a1aa0aeeef0";
    public static final String OBSERVER_ACCOUNT_1_IS_INNER_GROUP_ID = "cdabf434-ce26-43d1-8593-c65cffdbd7ef";
    public static final String OBSERVER_ACCOUNT_2_IS_NOT_INNER_GROUP_ID = "137b5eb2-b406-45ce-9652-69fdc9ba126e";
    public static final String OBSERVER_ACCOUNT_2_IS_INNER_GROUP_ID = "664e8df9-cdb0-48f2-9d1a-61a81646bc55";


    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http
                .httpBasic().disable()
                .cors().and()
                .csrf().disable()
                .authorizeRequests()
                .antMatchers(SecurityConfig.AUTH_WHITELIST_PERMIT_ALL).permitAll()
                .antMatchers(SecurityConfig.AUTH_WHITELIST_ANONYMOUS).anonymous()
                .anyRequest().authenticated().and()
                .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS);
        http.headers().frameOptions().disable();
        http.headers().xssProtection();
    }

    @Bean
    CorsConfigurationSource corsConfigurationSource() {
        return SecurityConfig.getCorsConfiguration();
    }

    @Bean
    public UserDetailsService userDetailsTestService() {
        return username -> {
            JwtUser jwtUser;
            jwtUser = switch (username) {
                case "OWNER_ACCOUNT_1_IS_INNER_GROUP" ->
                        getJwtUser(OWNER_ACCOUNT_1_IS_INNER_GROUP_ID, ACCOUNT_1_ID, ROLE_ACCOUNT_OWNER, WEIGHT_ROLE_50, INNER_GROUP_ID, true);
                case "OWNER_ACCOUNT_2_IS_INNER_GROUP" ->
                        getJwtUser(OWNER_ACCOUNT_2_IS_INNER_GROUP_ID, ACCOUNT_2_ID, ROLE_ACCOUNT_OWNER, WEIGHT_ROLE_50, INNER_GROUP_ID, true);
                case "ADMIN_ACCOUNT_1_IS_INNER_GROUP" ->
                        getJwtUser(ADMIN_ACCOUNT_1_IS_INNER_GROUP_ID, ACCOUNT_1_ID, ROLE_ADMIN, WEIGHT_ROLE_40, INNER_GROUP_ID, true);
                case "ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP" ->
                        getJwtUser(ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP_ID, ACCOUNT_1_ID, ROLE_ADMIN, WEIGHT_ROLE_40, NOT_INNER_GROUP_ID, false);
                case "ADMIN_ACCOUNT_2_IS_INNER_GROUP" ->
                        getJwtUser(ADMIN_ACCOUNT_2_IS_INNER_GROUP_ID, ACCOUNT_2_ID, ROLE_ADMIN, WEIGHT_ROLE_40, INNER_GROUP_ID, true);
                case "ADMIN_ACCOUNT_2_IS_NOT_INNER_GROUP" ->
                        getJwtUser(ADMIN_ACCOUNT_2_IS_NOT_INNER_GROUP_ID, ACCOUNT_2_ID, ROLE_ADMIN, WEIGHT_ROLE_40, NOT_INNER_GROUP_ID, false);
                case "EXECUTOR_ACCOUNT_1_IS_INNER_GROUP" ->
                        getJwtUser(EXECUTOR_ACCOUNT_1_IS_INNER_GROUP_ID, ACCOUNT_1_ID, ROLE_EXECUTOR, WEIGHT_ROLE_30, INNER_GROUP_ID, true);
                case "EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP" ->
                        getJwtUser(EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP_ID, ACCOUNT_1_ID, ROLE_EXECUTOR, WEIGHT_ROLE_30, NOT_INNER_GROUP_ID, false);
                case "EXECUTOR_ACCOUNT_2_IS_INNER_GROUP" ->
                        getJwtUser(EXECUTOR_ACCOUNT_2_IS_INNER_GROUP_ID, ACCOUNT_2_ID, ROLE_EXECUTOR, WEIGHT_ROLE_30, INNER_GROUP_ID, true);
                case "EXECUTOR_ACCOUNT_2_IS_NOT_INNER_GROUP" ->
                        getJwtUser(EXECUTOR_ACCOUNT_2_IS_NOT_INNER_GROUP_ID, ACCOUNT_2_ID, ROLE_EXECUTOR, WEIGHT_ROLE_30, NOT_INNER_GROUP_ID, false);
                case "AUTHOR_ACCOUNT_1_IS_INNER_GROUP" ->
                        getJwtUser(AUTHOR_ACCOUNT_1_IS_INNER_GROUP_ID, ACCOUNT_1_ID, ROLE_AUTHOR, WEIGHT_ROLE_20, INNER_GROUP_ID, true);
                case "AUTHOR_ACCOUNT_1_IS_NOT_INNER_GROUP" ->
                        getJwtUser(AUTHOR_ACCOUNT_1_IS_NOT_INNER_GROUP_ID, ACCOUNT_1_ID, ROLE_AUTHOR, WEIGHT_ROLE_20, NOT_INNER_GROUP_ID, false);
                case "AUTHOR_ACCOUNT_2_IS_INNER_GROUP" ->
                        getJwtUser(AUTHOR_ACCOUNT_2_IS_INNER_GROUP_ID, ACCOUNT_2_ID, ROLE_AUTHOR, WEIGHT_ROLE_20, INNER_GROUP_ID, true);
                case "AUTHOR_ACCOUNT_2_IS_NOT_INNER_GROUP" ->
                        getJwtUser(AUTHOR_ACCOUNT_2_IS_NOT_INNER_GROUP_ID, ACCOUNT_2_ID, ROLE_AUTHOR, WEIGHT_ROLE_20, NOT_INNER_GROUP_ID, false);
                case "OBSERVER_ACCOUNT_1_IS_INNER_GROUP" ->
                        getJwtUser(OBSERVER_ACCOUNT_1_IS_INNER_GROUP_ID, ACCOUNT_1_ID, ROLE_OBSERVER, WEIGHT_ROLE_10, INNER_GROUP_ID, true);
                case "OBSERVER_ACCOUNT_1_IS_NOT_INNER_GROUP" ->
                        getJwtUser(OBSERVER_ACCOUNT_1_IS_NOT_INNER_GROUP_ID, ACCOUNT_1_ID, ROLE_OBSERVER, WEIGHT_ROLE_10, NOT_INNER_GROUP_ID, false);
                case "OBSERVER_ACCOUNT_2_IS_INNER_GROUP" ->
                        getJwtUser(OBSERVER_ACCOUNT_2_IS_INNER_GROUP_ID, ACCOUNT_2_ID, ROLE_OBSERVER, WEIGHT_ROLE_10, INNER_GROUP_ID, true);
                case "OBSERVER_ACCOUNT_2_IS_NOT_INNER_GROUP" ->
                        getJwtUser(OBSERVER_ACCOUNT_2_IS_NOT_INNER_GROUP_ID, ACCOUNT_2_ID, ROLE_OBSERVER, WEIGHT_ROLE_10, NOT_INNER_GROUP_ID, false);
                default -> null;
            };
            return jwtUser;
        };
    }

    private JwtUser getJwtUser(String id, String accountId, String roleName, int weightRole, String groupId, boolean isInnerGroup) {
        return JwtUser.builder()
                .id(UUID.fromString(id))
                .accountId(UUID.fromString(accountId))
                .groupId(UUID.fromString(groupId))
                .weightRole(weightRole)
                .isInnerGroup(isInnerGroup)
                .username(EMAIL_1)
                .enabled(true)
                .authorities(List.of(new SimpleGrantedAuthority(roleName)))
                .build();
    }
}
