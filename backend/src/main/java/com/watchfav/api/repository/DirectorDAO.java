package com.watchfav.api.repository;

import com.watchfav.api.model.Country;
import com.watchfav.api.model.Director;
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
public class DirectorDAO {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Autowired
    private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

    private final RowMapper<Director> directorRowMapper = (rs, rowNum) -> {
        Country country = new Country(
                rs.getLong("country_id"),
                rs.getString("country_name"),
                rs.getBoolean("c_is_available")
        );

        Director director = new Director();
        director.setId(rs.getLong("director_id"));
        director.setName(rs.getString("director_name"));
        director.setIsAvailable(rs.getBoolean("d_is_available"));
        director.setCountry(country);

        return director;
    };

    public Director save(Director director) {
        if (director.getId() == null) {
            final String sql = "INSERT INTO tb_directors (director_name, country_id, is_available) VALUES (?, ?, ?)";
            KeyHolder keyHolder = new GeneratedKeyHolder();

            jdbcTemplate.update(connection -> {
                PreparedStatement ps = connection.prepareStatement(sql, new String[]{"director_id"});
                ps.setString(1, director.getName());
                ps.setLong(2, director.getCountry().getId());
                ps.setBoolean(3, director.getIsAvailable());
                return ps;
            }, keyHolder);

            director.setId(keyHolder.getKeyAs(Long.class));

        } else {
            final String sql = "UPDATE tb_directors SET director_name = ?, country_id = ?, is_available = ? WHERE director_id = ?";
            jdbcTemplate.update(sql,
                    director.getName(),
                    director.getCountry().getId(),
                    director.getIsAvailable(),
                    director.getId()
            );
        }
        return director;
    }

    public List<Director> findAllById(List<Long> ids) {
        if (ids == null || ids.isEmpty()) {
            return Collections.emptyList();
        }

        final String sql = "SELECT " +
                "d.director_id, d.director_name, d.is_available AS d_is_available, " +
                "c.country_id, c.country_name, c.is_available AS c_is_available " +
                "FROM tb_directors d INNER JOIN tb_countries c ON d.country_id = c.country_id " +
                "WHERE d.director_id IN (:ids)";

        MapSqlParameterSource parameters = new MapSqlParameterSource();
        parameters.addValue("ids", ids);

        return namedParameterJdbcTemplate.query(sql, parameters, directorRowMapper);
    }

    public Optional<Director> findById(Long id) {
        final String sql = "SELECT " +
                "d.director_id, d.director_name, d.is_available AS d_is_available, " +
                "c.country_id, c.country_name, c.is_available AS c_is_available " +
                "FROM tb_directors d INNER JOIN tb_countries c ON d.country_id = c.country_id " +
                "WHERE d.director_id = ?";
        try {
            Director director = jdbcTemplate.queryForObject(sql, directorRowMapper, id);
            return Optional.ofNullable(director);
        } catch (EmptyResultDataAccessException e) {
            return Optional.empty();
        }
    }

    public List<Director> findAllAvailableAndSort() {
        final String sql = "SELECT " +
                "d.director_id, d.director_name, d.is_available AS d_is_available, " +
                "c.country_id, c.country_name, c.is_available AS c_is_available " +
                "FROM tb_directors d INNER JOIN tb_countries c ON d.country_id = c.country_id " +
                "WHERE d.is_available = true ORDER BY d.director_name";
        return jdbcTemplate.query(sql, directorRowMapper);
    }

    public void updateIsAvailable(Long id, Boolean isAvailable) {
        final String sql = "UPDATE tb_directors SET is_available = ? WHERE director_id = ?";
        jdbcTemplate.update(sql, isAvailable, id);
    }
}
