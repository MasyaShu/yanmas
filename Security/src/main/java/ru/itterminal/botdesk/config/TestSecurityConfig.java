package ru.itterminal.botdesk.config;

import static ru.itterminal.botdesk.config.SecurityConfig.AUTH_WHITELIST_ANONYMOUS;
import static ru.itterminal.botdesk.config.SecurityConfig.AUTH_WHITELIST_PERMIT_ALL;

import java.util.List;
import java.util.UUID;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Profile;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.web.cors.CorsConfigurationSource;

import ru.itterminal.botdesk.jwt.JwtUser;

@EnableGlobalMethodSecurity(securedEnabled = true, prePostEnabled = true)
@EnableWebSecurity
@Profile("Test")
public class TestSecurityConfig extends WebSecurityConfigurerAdapter {

    public static String ACCOUNT_1_ID = "cdfa6483-0769-4628-ba32-efd338a716de";
    public static String ACCOUNT_2_ID = "bcf98101-2a22-42bf-94cc-c900b50a0b69";
    public static String GROUP_1_ID = "0223e51a-4bb2-44ee-bc8e-1f047a2145e7";
    public static String GROUP_2_ID = "99f6b488-7687-4451-b8a1-9fbeb2a3efec";
    public static String EMAIL_1 = "m@m.ru";
    public static String PASSWORD = "12345";
    public static String EMAIL_2 = "m4@m.ru";

    protected void configure(HttpSecurity http) throws Exception {
        http
                .httpBasic().disable()
                .cors().and()
                .csrf().disable()
                .authorizeRequests()
                .antMatchers(AUTH_WHITELIST_PERMIT_ALL).permitAll()
                .antMatchers(AUTH_WHITELIST_ANONYMOUS).anonymous()
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
    public UserDetailsService UserDetailsTestService() {
        return new  UserDetailsService() {
            @Override
            public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
                JwtUser jwtUser = null;
                switch (username) {
                    case "OWNER_ACCOUNT_1_IS_INNER_GROUP" :
                        jwtUser = new JwtUser()
                                .builder()
                                .accountId(UUID.fromString(ACCOUNT_1_ID))
                                .weightRole(3)
                                .username(EMAIL_1)
                                .is_inner_group(true)
                                .enabled(true)
                                .authorities(List.of(new SimpleGrantedAuthority("ACCOUNT_OWNER")))
                                .build();
                        break;
                    case "OWNER_ACCOUNT_2_IS_INNER_GROUP" :
                        jwtUser = new JwtUser()
                                .builder()
                                .accountId(UUID.fromString(ACCOUNT_2_ID))
                                .weightRole(3)
                                .username(EMAIL_1)
                                .is_inner_group(true)
                                .enabled(true)
                                .authorities(List.of(new SimpleGrantedAuthority("ACCOUNT_OWNER")))
                                .build();
                        break;
                    case "ADMIN_ACCOUNT_1_IS_INNER_GROUP" :
                        jwtUser = new JwtUser()
                                .builder()
                                .accountId(UUID.fromString(ACCOUNT_1_ID))
                                .weightRole(2)
                                .is_inner_group(true)
                                .username(EMAIL_1)
                                .enabled(true)
                                .authorities(List.of(new SimpleGrantedAuthority("ADMIN")))
                                .build();
                        break;
                    case "ADMIN_ACCOUNT_2_IS_INNER_GROUP" :
                        jwtUser = new JwtUser()
                                .builder()
                                .accountId(UUID.fromString(ACCOUNT_2_ID))
                                .weightRole(2)
                                .is_inner_group(true)
                                .username(EMAIL_1)
                                .enabled(true)
                                .authorities(List.of(new SimpleGrantedAuthority("ADMIN")))
                                .build();
                        break;
                    case "AUTHOR_ACCOUNT_1_IS_INNER_GROUP" :
                        jwtUser = new JwtUser()
                                .builder()
                                .accountId(UUID.fromString(ACCOUNT_1_ID))
                                .weightRole(0)
                                .is_inner_group(true)
                                .username(EMAIL_1)
                                .enabled(true)
                                .authorities(List.of(new SimpleGrantedAuthority("AUTHOR")))
                                .build();
                        break;
                    case "ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP" :
                        jwtUser = new JwtUser()
                                .builder()
                                .accountId(UUID.fromString(ACCOUNT_1_ID))
                                .weightRole(2)
                                .is_inner_group(false)
                                .username(EMAIL_1)
                                .enabled(true)
                                .authorities(List.of(new SimpleGrantedAuthority("ADMIN")))
                                .build();
                        break;
                    case "ADMIN_ACCOUNT_2_IS_NOT_INNER_GROUP" :
                        jwtUser = new JwtUser()
                                .builder()
                                .accountId(UUID.fromString(ACCOUNT_2_ID))
                                .weightRole(2)
                                .is_inner_group(false)
                                .username(EMAIL_1)
                                .enabled(true)
                                .authorities(List.of(new SimpleGrantedAuthority("ADMIN")))
                                .build();
                        break;
                    case "AUTHOR_ACCOUNT_1_IS_NOT_INNER_GROUP" :
                        jwtUser = new JwtUser()
                                .builder()
                                .accountId(UUID.fromString(ACCOUNT_1_ID))
                                .groupId(UUID.fromString(GROUP_1_ID))
                                .weightRole(0)
                                .is_inner_group(false)
                                .username(EMAIL_1)
                                .enabled(true)
                                .authorities(List.of(new SimpleGrantedAuthority("AUTHOR")))
                                .build();
                        break;
                    case "EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP" :
                        jwtUser = new JwtUser()
                                .builder()
                                .accountId(UUID.fromString(ACCOUNT_1_ID))
                                .groupId(UUID.fromString(GROUP_1_ID))
                                .weightRole(1)
                                .is_inner_group(false)
                                .username(EMAIL_1)
                                .enabled(true)
                                .authorities(List.of(new SimpleGrantedAuthority("EXECUTOR")))
                                .build();
                        break;
                }

                return jwtUser;

            }
        };
    }

}
