package com.watchfav.api.repository;

import com.watchfav.api.model.Genre;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface GenreRepository extends JpaRepository<Genre, Long> {
    @Query("SELECT g FROM Genre g WHERE g.isAvailable = true ORDER BY g.name")
    List<Genre> findAllByAvailableAndSort();

    @Query("SELECT g FROM Genre g WHERE LOWER(g.name) LIKE LOWER(CONCAT('%', :text, '%')) AND g.isAvailable = true")
    List<Genre> findAllBySearch(@Param("text") String text);
}