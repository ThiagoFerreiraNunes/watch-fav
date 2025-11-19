package com.watchfav.api.repository;

import com.watchfav.api.model.Series;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface SeriesRepository extends JpaRepository<Series, Long> {
    @Query("SELECT s FROM Series s WHERE s.isAvailable = true")
    Page<Series> findAllByAvailable(Pageable pageable);
}
