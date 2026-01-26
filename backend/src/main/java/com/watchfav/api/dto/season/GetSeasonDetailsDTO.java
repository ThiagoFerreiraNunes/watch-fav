package com.watchfav.api.dto.season;

import com.watchfav.api.model.Season;

public record GetSeasonDetailsDTO(
        Long id,
        Integer number,
        Integer releaseYear,
        Integer episodes,
        String series
) {
    public GetSeasonDetailsDTO(Season season){
        this(
                season.getId(),
                season.getNumber(),
                season.getReleaseYear(),
                season.getEpisodes(),
                season.getSeries().getName()
        );
    }
}
