package ru.itterminal.botdesk.config;

import static ru.itterminal.botdesk.config.SecurityConfig.AUTH_WHITELIST;

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
    public static String EMAIL_2 = "m4@m.ru";

    protected void configure(HttpSecurity http) throws Exception {
        http
                .httpBasic().disable()
                .cors().and()
                .csrf().disable()
                .authorizeRequests()
                .antMatchers(AUTH_WHITELIST).permitAll()
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
            public UserDetails loadUserByUsername(String role) throws UsernameNotFoundException {
                JwtUser jwtUser = null;
                switch (role) {
                    case "ADMIN_ACCOUNT_1" :
                        jwtUser = new JwtUser()
                                .builder()
                                .accountId(UUID.fromString(ACCOUNT_1_ID))
                                .username(EMAIL_1)
                                .enabled(true)
                                .authorities(List.of(new SimpleGrantedAuthority("ADMIN")))
                                .build();
                        break;
                    case "ADMIN_ACCOUNT_2" :
                        jwtUser = new JwtUser()
                                .builder()
                                .accountId(UUID.fromString(ACCOUNT_2_ID))
                                .username(EMAIL_1)
                                .enabled(true)
                                .authorities(List.of(new SimpleGrantedAuthority("ADMIN")))
                                .build();
                        break;
                    case "AUTHOR_ACCOUNT_1" :
                        jwtUser = new JwtUser()
                                .builder()
                                .accountId(UUID.fromString(ACCOUNT_1_ID))
                                .username(EMAIL_1)
                                .enabled(true)
                                .authorities(List.of(new SimpleGrantedAuthority("AUTHOR")))
                                .build();
                        break;
                }

                return jwtUser;

            }
        };
    }

}
