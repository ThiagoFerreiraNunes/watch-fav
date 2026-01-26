package com.watchfav.api.repository;

import com.watchfav.api.model.Actor;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface ActorRepository extends JpaRepository<Actor, Long> {
    @Query("SELECT a FROM Actor a WHERE a.isAvailable = true ORDER BY a.name")
    List<Actor> findAllByAvailableAndSort();

    @Query("SELECT a FROM Actor a WHERE LOWER(a.name) LIKE LOWER(CONCAT('%', :text, '%')) AND a.isAvailable = true")
    List<Actor> findAllBySearch(@Param("text") String text);
}
