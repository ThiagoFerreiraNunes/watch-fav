package com.watchfav.api.dto.season;

import com.watchfav.api.model.Season;

public record GetSeasonDTO(
        Long id,
        Integer number,
        Integer releaseYear,
        Integer episodes
) {
    public GetSeasonDTO(Season season){
        this(
                season.getId(),
                season.getNumber(),
                season.getReleaseYear(),
                season.getEpisodes()
        );
    }
}
