package com.watchfav.api.repository;

import com.watchfav.api.dto.movies.GetMovieDTO;
import com.watchfav.api.model.Movie;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface MovieRepository extends JpaRepository<Movie, Long> {
    @Query("SELECT m FROM Movie m WHERE m.isAvailable = true")
    Page<Movie> findAllByAvailable(Pageable pageable);

    @Query("SELECT m FROM Movie m WHERE LOWER(m.name) LIKE LOWER(CONCAT('%', :text, '%')) AND m.isAvailable = true")
    Page<Movie> findAllBySearch(@Param("text") String text, Pageable pageable);
}
