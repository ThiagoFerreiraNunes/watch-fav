package com.watchfav.api.repository;

import com.watchfav.api.model.Episode;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface EpisodeRepository extends JpaRepository<Episode, Long> {
    @Query("SELECT e FROM Episode e WHERE e.season.id = :seasonId AND e.isAvailable = true")
    List<Episode> findAllBySeasonIdAndAvailable(@Param("seasonId") Long seasonId);

    @Query("SELECT e FROM Episode e " +
            "JOIN FETCH e.season s " +
            "JOIN FETCH s.series " +
            "WHERE s.id = :seasonId AND e.id = :episodeId")
    Episode findBySeasonIdAndEpisodeId(@Param("seasonId") Long seasonId, @Param("episodeId") Long episodeId);
}
