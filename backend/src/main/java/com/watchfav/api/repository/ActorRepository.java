package com.watchfav.api.repository;

import com.watchfav.api.model.Actor;
import com.watchfav.api.model.Country;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
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

public interface ActorRepository extends JpaRepository<Actor, Long> {
    @Query("SELECT a FROM Actor a WHERE a.isAvailable = true ORDER BY a.name")
    List<Actor> findAllByAvailableAndSort();
}
