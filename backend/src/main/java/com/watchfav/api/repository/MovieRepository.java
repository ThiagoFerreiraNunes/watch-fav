package com.watchfav.api.repository;

import com.watchfav.api.model.Movie;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface MovieRepository extends JpaRepository<Movie, Long> {
    @Query("SELECT m FROM Movie m WHERE m.isAvailable = true")
    Page<Movie> findAllByAvailable(Pageable pageable);
}
