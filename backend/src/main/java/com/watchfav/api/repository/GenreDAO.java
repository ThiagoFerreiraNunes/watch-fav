package com.watchfav.api.repository;

import com.watchfav.api.model.Genre;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.stereotype.Repository;

import java.sql.PreparedStatement;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Repository
public class GenreDAO {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Autowired
    private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

    private final RowMapper<Genre> rowMapper = (rs, rowNum) -> new Genre(
            rs.getLong("genre_id"),
            rs.getString("genre_name"),
            rs.getBoolean("is_available")
    );

    public Genre save(Genre genre) {
        if (genre.getId() == null) {
            final String sql = "INSERT INTO tb_genres (genre_name, is_available) VALUES (?, ?)";
            KeyHolder keyHolder = new GeneratedKeyHolder();

            jdbcTemplate.update(connection -> {
                PreparedStatement ps = connection.prepareStatement(sql, new String[]{"genre_id"});
                ps.setString(1, genre.getName());
                ps.setBoolean(2, genre.getIsAvailable());
                return ps;
            }, keyHolder);

            genre.setId(keyHolder.getKeyAs(Long.class));

        } else {
            final String sql = "UPDATE tb_genres SET genre_name = ?, is_available = ? WHERE genre_id = ?";
            jdbcTemplate.update(sql,
                    genre.getName(),
                    genre.getIsAvailable(),
                    genre.getId()
            );
        }
        return genre;
    }

    public List<Genre> findAllById(List<Long> ids) {
        if (ids == null || ids.isEmpty()) {
            return Collections.emptyList();
        }

        String sql = "SELECT * FROM tb_genres WHERE genre_id IN (:ids)";

        MapSqlParameterSource parameters = new MapSqlParameterSource();
        parameters.addValue("ids", ids);

        return namedParameterJdbcTemplate.query(sql, parameters, rowMapper);
    }

    public Optional<Genre> findById(Long id) {
        final String sql = "SELECT * FROM tb_genres WHERE genre_id = ?";
        try {
            Genre genre = jdbcTemplate.queryForObject(sql, rowMapper, id);
            return Optional.ofNullable(genre);
        } catch (EmptyResultDataAccessException e) {
            return Optional.empty();
        }
    }

    public List<Genre> findAllAvailableAndSort() {
        final String sql = "SELECT * FROM tb_genres WHERE is_available = true ORDER BY genre_name";
        return jdbcTemplate.query(sql, rowMapper);
    }

    public void updateIsAvailable(Long id, Boolean isAvailable) {
        final String sql = "UPDATE tb_genres SET is_available = ? WHERE genre_id = ?";
        jdbcTemplate.update(sql, isAvailable, id);
    }
}