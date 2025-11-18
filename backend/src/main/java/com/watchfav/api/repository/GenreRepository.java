package com.watchfav.api.repository;

import com.watchfav.api.model.Genre;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface GenreRepository extends JpaRepository<Genre, Long> {
    @Query("SELECT g FROM Genre g WHERE g.isAvailable = true ORDER BY g.name")
    List<Genre> findAllByAvailableAndSort();
}