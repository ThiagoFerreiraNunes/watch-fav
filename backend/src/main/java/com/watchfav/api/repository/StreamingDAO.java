package com.watchfav.api.repository;

import com.watchfav.api.model.Streaming;
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
public class StreamingDAO {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Autowired
    private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

    private final RowMapper<Streaming> rowMapper = (rs, rowNum) -> new Streaming(
            rs.getLong("streaming_id"),
            rs.getString("streaming_name"),
            rs.getString("streaming_url"),
            rs.getBoolean("is_available")
    );

    public Streaming save(Streaming streaming) {
        if (streaming.getId() == null) {
            final String sql = "INSERT INTO tb_streamings (streaming_name, streaming_url, is_available) VALUES (?, ?, ?)";
            KeyHolder keyHolder = new GeneratedKeyHolder();

            jdbcTemplate.update(connection -> {
                PreparedStatement ps = connection.prepareStatement(sql, new String[]{"streaming_id"});
                ps.setString(1, streaming.getName());
                ps.setString(2, streaming.getUrl());
                ps.setBoolean(3, streaming.getIsAvailable());
                return ps;
            }, keyHolder);

            streaming.setId(keyHolder.getKeyAs(Long.class));

        } else {
            final String sql = "UPDATE tb_streamings SET streaming_name = ?, streaming_url = ?, is_available = ? WHERE streaming_id = ?";
            jdbcTemplate.update(sql,
                    streaming.getName(),
                    streaming.getUrl(),
                    streaming.getIsAvailable(),
                    streaming.getId()
            );
        }
        return streaming;
    }

    public List<Streaming> findAllById(List<Long> ids) {
        if (ids == null || ids.isEmpty()) {
            return Collections.emptyList();
        }

        String sql = "SELECT * FROM tb_streamings WHERE streaming_id IN (:ids)";

        MapSqlParameterSource parameters = new MapSqlParameterSource();
        parameters.addValue("ids", ids);

        return namedParameterJdbcTemplate.query(sql, parameters, rowMapper);
    }

    public Optional<Streaming> findById(Long id) {
        final String sql = "SELECT * FROM tb_streamings WHERE streaming_id = ?";
        try {
            Streaming streaming = jdbcTemplate.queryForObject(sql, rowMapper, id);
            return Optional.ofNullable(streaming);
        } catch (EmptyResultDataAccessException e) {
            return Optional.empty();
        }
    }

    public List<Streaming> findAllAvailableAndSort() {
        final String sql = "SELECT * FROM tb_streamings WHERE is_available = true ORDER BY streaming_name";
        return jdbcTemplate.query(sql, rowMapper);
    }

    public void updateIsAvailable(Long id, Boolean isAvailable) {
        final String sql = "UPDATE tb_streamings SET is_available = ? WHERE streaming_id = ?";
        jdbcTemplate.update(sql, isAvailable, id);
    }
}
