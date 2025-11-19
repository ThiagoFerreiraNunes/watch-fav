package com.watchfav.api.dto.episode;

import com.watchfav.api.model.Episode;

import java.time.LocalTime;

public record GetEpisodeDTO(
        Long id,
        Integer number,
        String name,
        LocalTime duration
) {
    public GetEpisodeDTO(Episode episode){
        this(
                episode.getId(),
                episode.getNumber(),
                episode.getName(),
                episode.getDuration()
        );
    }
}

