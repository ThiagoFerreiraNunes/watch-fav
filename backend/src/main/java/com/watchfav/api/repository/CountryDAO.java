package com.watchfav.api.repository;

import com.watchfav.api.model.Country;
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
public class CountryDAO {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    private final RowMapper<Country> rowMapper = (rs, rowNum) -> new Country(
            rs.getLong("country_id"),
            rs.getString("country_name"),
            rs.getBoolean("is_available")
    );

    public Country save(Country country) {
        if (country.getId() == null) {
            final String sql = "INSERT INTO tb_countries (country_name, is_available) VALUES (?, ?)";
            KeyHolder keyHolder = new GeneratedKeyHolder();

            jdbcTemplate.update(connection -> {
                PreparedStatement ps = connection.prepareStatement(sql, new String[]{"country_id"});
                ps.setString(1, country.getName());
                ps.setBoolean(2, country.getIsAvailable());
                return ps;
            }, keyHolder);

            country.setId(keyHolder.getKeyAs(Long.class));

        } else {
            final String sql = "UPDATE tb_countries SET country_name = ?, is_available = ? WHERE country_id = ?";
            jdbcTemplate.update(sql,
                    country.getName(),
                    country.getIsAvailable(),
                    country.getId()
            );
        }
        return country;
    }

    public Optional<Country> findById(Long id) {
        final String sql = "SELECT * FROM tb_countries WHERE country_id = ?";
        try {
            Country country = jdbcTemplate.queryForObject(sql, rowMapper, id);
            return Optional.ofNullable(country);
        } catch (EmptyResultDataAccessException e) {
            return Optional.empty();
        }
    }

    public List<Country> findAllAvailableAndSort() {
        final String sql = "SELECT * FROM tb_countries WHERE is_available = true ORDER BY country_name";
        return jdbcTemplate.query(sql, rowMapper);
    }

    public void updateIsAvailable(Long id, Boolean isAvailable) {
        final String sql = "UPDATE tb_countries SET is_available = ? WHERE country_id = ?";
        jdbcTemplate.update(sql, isAvailable, id);
    }
}
