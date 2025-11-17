package com.watchfav.api.repository;

import com.watchfav.api.model.Language;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.stereotype.Repository;

import java.sql.PreparedStatement;
import java.util.List;
import java.util.Optional;

@Repository
public class LanguageDAO {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    private final RowMapper<Language> rowMapper = (rs, rowNum) -> new Language(
            rs.getLong("language_id"),
            rs.getString("language_name"),
            rs.getBoolean("is_available")
    );

    public Language save(Language language) {
        if (language.getId() == null) {
            final String sql = "INSERT INTO tb_languages (language_name, is_available) VALUES (?, ?)";
            KeyHolder keyHolder = new GeneratedKeyHolder();

            jdbcTemplate.update(connection -> {
                PreparedStatement ps = connection.prepareStatement(sql, new String[]{"language_id"});
                ps.setString(1, language.getName());
                ps.setBoolean(2, language.getIsAvailable());
                return ps;
            }, keyHolder);

            language.setId(keyHolder.getKeyAs(Long.class));

        } else {
            final String sql = "UPDATE tb_languages SET language_name = ?, is_available = ? WHERE language_id = ?";
            jdbcTemplate.update(sql,
                    language.getName(),
                    language.getIsAvailable(),
                    language.getId()
            );
        }
        return language;
    }

    public Optional<Language> findById(Long id) {
        final String sql = "SELECT * FROM tb_languages WHERE language_id = ?";
        try {
            Language language = jdbcTemplate.queryForObject(sql, rowMapper, id);
            return Optional.ofNullable(language);
        } catch (EmptyResultDataAccessException e) {
            return Optional.empty();
        }
    }

    public List<Language> findAllAvailableAndSort() {
        final String sql = "SELECT * FROM tb_languages WHERE is_available = true ORDER BY language_name";
        return jdbcTemplate.query(sql, rowMapper);
    }

    public void updateIsAvailable(Long id, Boolean isAvailable) {
        final String sql = "UPDATE tb_languages SET is_available = ? WHERE language_id = ?";
        jdbcTemplate.update(sql, isAvailable, id);
    }
}
