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
                        getJwtUser(ACCOUNT_1_ID, ROLE_ACCOUNT_OWNER, WEIGHT_ROLE_50, INNER_GROUP_ID, true);
                case "OWNER_ACCOUNT_2_IS_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_2_ID, ROLE_ACCOUNT_OWNER, WEIGHT_ROLE_50, INNER_GROUP_ID, true);
                case "ADMIN_ACCOUNT_1_IS_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_1_ID, ROLE_ADMIN, WEIGHT_ROLE_40, INNER_GROUP_ID, true);
                case "ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_1_ID, ROLE_ADMIN, WEIGHT_ROLE_40, NOT_INNER_GROUP_ID, false);
                case "ADMIN_ACCOUNT_2_IS_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_2_ID, ROLE_ADMIN, WEIGHT_ROLE_40, INNER_GROUP_ID, true);
                case "ADMIN_ACCOUNT_2_IS_NOT_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_2_ID, ROLE_ADMIN, WEIGHT_ROLE_40, NOT_INNER_GROUP_ID, false);
                case "EXECUTOR_ACCOUNT_1_IS_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_1_ID, ROLE_EXECUTOR, WEIGHT_ROLE_30, INNER_GROUP_ID, true);
                case "EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_1_ID, ROLE_EXECUTOR, WEIGHT_ROLE_30, NOT_INNER_GROUP_ID, false);
                case "EXECUTOR_ACCOUNT_2_IS_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_2_ID, ROLE_EXECUTOR, WEIGHT_ROLE_30, INNER_GROUP_ID, true);
                case "EXECUTOR_ACCOUNT_2_IS_NOT_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_2_ID, ROLE_EXECUTOR, WEIGHT_ROLE_30, NOT_INNER_GROUP_ID, false);
                case "AUTHOR_ACCOUNT_1_IS_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_1_ID, ROLE_AUTHOR, WEIGHT_ROLE_20, INNER_GROUP_ID, true);
                case "AUTHOR_ACCOUNT_1_IS_NOT_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_1_ID, ROLE_AUTHOR, WEIGHT_ROLE_20, NOT_INNER_GROUP_ID, false);
                case "AUTHOR_ACCOUNT_2_IS_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_2_ID, ROLE_AUTHOR, WEIGHT_ROLE_20, INNER_GROUP_ID, true);
                case "AUTHOR_ACCOUNT_2_IS_NOT_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_2_ID, ROLE_AUTHOR, WEIGHT_ROLE_20, NOT_INNER_GROUP_ID, false);
                case "OBSERVER_ACCOUNT_1_IS_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_1_ID, ROLE_OBSERVER, WEIGHT_ROLE_10, INNER_GROUP_ID, true);
                case "OBSERVER_ACCOUNT_1_IS_NOT_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_1_ID, ROLE_OBSERVER, WEIGHT_ROLE_10, NOT_INNER_GROUP_ID, false);
                case "OBSERVER_ACCOUNT_2_IS_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_2_ID, ROLE_OBSERVER, WEIGHT_ROLE_10, INNER_GROUP_ID, true);
                case "OBSERVER_ACCOUNT_2_IS_NOT_INNER_GROUP" ->
                        getJwtUser(ACCOUNT_2_ID, ROLE_OBSERVER, WEIGHT_ROLE_10, NOT_INNER_GROUP_ID, false);
                default -> null;
            };
            return jwtUser;
        };
    }

    private JwtUser getJwtUser(String accountId, String roleName, int weightRole, String groupId, boolean isInnerGroup) {
        return JwtUser.builder()
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
