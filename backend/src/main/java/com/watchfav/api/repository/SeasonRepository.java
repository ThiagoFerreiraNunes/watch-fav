package com.watchfav.api.repository;

import com.watchfav.api.model.Season;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface SeasonRepository extends JpaRepository<Season, Long> {
    @Query("SELECT s FROM Season s WHERE s.series.id = :seriesId AND s.isAvailable = true")
    List<Season> findAllBySeriesIdAndAvailable(@Param("seriesId") Long seriesId);

    @Query("SELECT s FROM Season s WHERE s.series.id = :seriesId AND s.id = :seasonId")
    Season findBySeriesIdAndSeasonId(@Param("seriesId") Long seriesId, @Param("seasonId") Long seasonId);
}
