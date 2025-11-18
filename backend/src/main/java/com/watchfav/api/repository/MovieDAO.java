package com.watchfav.api.repository;

import com.watchfav.api.model.AgeRating;
import com.watchfav.api.model.Country;
import com.watchfav.api.model.Movie;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.stereotype.Repository;

import java.sql.PreparedStatement;
import java.time.LocalTime;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Repository
public class MovieDAO {

    @Autowired
    private JdbcTemplate jdbcTemplate;
    @Autowired
    private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

    private final RowMapper<Movie> movieRowMapper = (rs, rowNum) -> {
        Country country = new Country(
                rs.getLong("country_id"),
                rs.getString("country_name"),
                rs.getBoolean("c_is_available")
        );

        return new Movie(
                rs.getLong("movie_id"),
                rs.getString("movie_name"),
                rs.getString("image_url"),
                rs.getString("movie_description"),
                rs.getInt("release_year"),
                rs.getObject("duration", LocalTime.class),
                AgeRating.valueOf(rs.getString("age_rating")),
                country,
                rs.getBoolean("m_is_available")
        );
    };

    private static final String BASE_SELECT_SQL =
            "SELECT m.*, m.is_available AS m_is_available, " +
                    "c.country_id, c.country_name, c.is_available AS c_is_available " +
                    "FROM tb_movies m JOIN tb_countries c ON m.country_id = c.country_id ";

    public Movie save(Movie movie) {
        if (movie.getId() == null) {
            final String sql = "INSERT INTO tb_movies (movie_name, image_url, movie_description, release_year, duration, age_rating, country_id, is_available) VALUES (?, ?, ?, ?, ?, ?, ?, ?)";
            KeyHolder keyHolder = new GeneratedKeyHolder();

            jdbcTemplate.update(connection -> {
                PreparedStatement ps = connection.prepareStatement(sql, new String[]{"movie_id"});
                ps.setString(1, movie.getName());
                ps.setString(2, movie.getImageUrl());
                ps.setString(3, movie.getDescription());
                ps.setInt(4, movie.getReleaseYear());
                ps.setObject(5, movie.getDuration());
                ps.setString(6, movie.getAgeRating().name());
                ps.setLong(7, movie.getCountry().getId());
                ps.setBoolean(8, movie.getIsAvailable());
                return ps;
            }, keyHolder);

            movie.setId(keyHolder.getKeyAs(Long.class));
        } else {
            final String sql = "UPDATE tb_movies SET movie_name=?, image_url=?, movie_description=?, release_year=?, duration=?, age_rating=?, country_id=?, is_available=? WHERE movie_id=?";
            jdbcTemplate.update(sql,
                    movie.getName(), movie.getImageUrl(), movie.getDescription(), movie.getReleaseYear(),
                    movie.getDuration(), movie.getAgeRating().name(), movie.getCountry().getId(),
                    movie.getIsAvailable(), movie.getId());
        }
        return movie;
    }

    public Optional<Movie> findById(Long id) {
        final String sql = BASE_SELECT_SQL + "WHERE m.movie_id = ?";
        try {
            Movie movie = jdbcTemplate.queryForObject(sql, movieRowMapper, id);
            return Optional.ofNullable(movie);
        } catch (EmptyResultDataAccessException e) {
            return Optional.empty();
        }
    }

    public long countAllAvailable() {
        String sql = "SELECT COUNT(*) FROM tb_movies WHERE is_available = true";

        Long count = jdbcTemplate.queryForObject(sql, Long.class);

        if (count == null) {
            return 0L;
        }

        return count;
    }

    public Page<Movie> findAllAvailable(Pageable pageable) {
        String orderBy = pageable.getSort().stream()
                .map(order -> "m." + (order.getProperty().equals("name") ? "movie_name" : order.getProperty()) + " " + order.getDirection().name())
                .collect(Collectors.joining(", "));

        if (orderBy.isEmpty()) {
            orderBy = "m.movie_name ASC";
        }

        String sql = BASE_SELECT_SQL +
                "WHERE m.is_available = true " +
                "ORDER BY " + orderBy + " " +
                "LIMIT ? OFFSET ?";

        List<Movie> movies = jdbcTemplate.query(sql, movieRowMapper, pageable.getPageSize(), pageable.getOffset());

        long total = countAllAvailable();

        return new PageImpl<>(movies, pageable, total);
    }

    public void updateIsAvailable(Long id, Boolean isAvailable) {
        final String sql = "UPDATE tb_movies SET is_available = ? WHERE movie_id = ?";
        jdbcTemplate.update(sql, isAvailable, id);
    }

    private void deleteRelationships(Long movieId, String tableName) {
        String sql = "DELETE FROM " + tableName + " WHERE movie_id = ?";
        jdbcTemplate.update(sql, movieId);
    }

    private void insertRelationships(Long movieId, String tableName, String inverseIdColumn, List<Long> relatedIds) {
        if (relatedIds == null || relatedIds.isEmpty()) return;

        String sql = "INSERT INTO " + tableName + " (movie_id, " + inverseIdColumn + ") VALUES (?, ?)";

        jdbcTemplate.batchUpdate(sql, relatedIds, relatedIds.size(), (ps, id) -> {
            ps.setLong(1, movieId);
            ps.setLong(2, id);
        });
    }

    public void updateRelationships(Long movieId, String tableName, String inverseIdColumn, List<Long> relatedIds) {
        deleteRelationships(movieId, tableName);
        insertRelationships(movieId, tableName, inverseIdColumn, relatedIds);
    }

    public List<Long> findRelatedIds(Long movieId, String tableName, String inverseIdColumn) {
        String sql = "SELECT " + inverseIdColumn + " FROM " + tableName + " WHERE movie_id = ?";
        return jdbcTemplate.queryForList(sql, Long.class, movieId);
    }
}