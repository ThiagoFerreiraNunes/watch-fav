package com.watchfav.api.repository;

import com.watchfav.api.model.Actor;
import com.watchfav.api.model.Country;
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
public class ActorDAO {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Autowired
    private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

    private final RowMapper<Actor> actorRowMapper = (rs, rowNum) -> {
        Country country = new Country(
                rs.getLong("country_id"),
                rs.getString("country_name"),
                rs.getBoolean("c_is_available")
        );

        Actor actor = new Actor();
        actor.setId(rs.getLong("actor_id"));
        actor.setName(rs.getString("actor_name"));
        actor.setIsAvailable(rs.getBoolean("a_is_available"));
        actor.setCountry(country);

        return actor;
    };

    public Actor save(Actor actor) {
        if (actor.getId() == null) {
            final String sql = "INSERT INTO tb_actors (actor_name, country_id, is_available) VALUES (?, ?, ?)";
            KeyHolder keyHolder = new GeneratedKeyHolder();

            jdbcTemplate.update(connection -> {
                PreparedStatement ps = connection.prepareStatement(sql, new String[]{"actor_id"});
                ps.setString(1, actor.getName());
                ps.setLong(2, actor.getCountry().getId());
                ps.setBoolean(3, actor.getIsAvailable());
                return ps;
            }, keyHolder);

            actor.setId(keyHolder.getKeyAs(Long.class));

        } else {
            final String sql = "UPDATE tb_actors SET actor_name = ?, country_id = ?, is_available = ? WHERE actor_id = ?";
            jdbcTemplate.update(sql,
                    actor.getName(),
                    actor.getCountry().getId(),
                    actor.getIsAvailable(),
                    actor.getId()
            );
        }
        return actor;
    }

    public List<Actor> findAllById(List<Long> ids) {
        if (ids == null || ids.isEmpty()) {
            return Collections.emptyList();
        }

        final String sql = "SELECT " +
                "a.actor_id, a.actor_name, a.is_available AS a_is_available, " +
                "c.country_id, c.country_name, c.is_available AS c_is_available " +
                "FROM tb_actors a INNER JOIN tb_countries c ON a.country_id = c.country_id " +
                "WHERE a.actor_id IN (:ids)";

        MapSqlParameterSource parameters = new MapSqlParameterSource();
        parameters.addValue("ids", ids);

        return namedParameterJdbcTemplate.query(sql, parameters, actorRowMapper);
    }

    public Optional<Actor> findById(Long id) {
        final String sql = "SELECT " +
                "a.actor_id, a.actor_name, a.is_available AS a_is_available, " +
                "c.country_id, c.country_name, c.is_available AS c_is_available " +
                "FROM tb_actors a INNER JOIN tb_countries c ON a.country_id = c.country_id " +
                "WHERE a.actor_id = ?";
        try {
            Actor actor = jdbcTemplate.queryForObject(sql, actorRowMapper, id);
            return Optional.ofNullable(actor);
        } catch (EmptyResultDataAccessException e) {
            return Optional.empty();
        }
    }

    public List<Actor> findAllAvailableAndSort() {
        final String sql = "SELECT " +
                "a.actor_id, a.actor_name, a.is_available AS a_is_available, " +
                "c.country_id, c.country_name, c.is_available AS c_is_available " +
                "FROM tb_actors a INNER JOIN tb_countries c ON a.country_id = c.country_id " +
                "WHERE a.is_available = true ORDER BY a.actor_name";
        return jdbcTemplate.query(sql, actorRowMapper);
    }

    public void updateIsAvailable(Long id, Boolean isAvailable) {
        final String sql = "UPDATE tb_actors SET is_available = ? WHERE actor_id = ?";
        jdbcTemplate.update(sql, isAvailable, id);
    }
}
