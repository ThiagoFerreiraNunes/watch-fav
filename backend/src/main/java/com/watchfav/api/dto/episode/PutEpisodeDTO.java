package com.watchfav.api.dto.episode;

import java.time.LocalTime;
import java.util.List;

public record PutEpisodeDTO(
        Integer number,
        String name,
        LocalTime duration,
        List<Long> mainActorIds,
        List<Long> directorIds
) {
}