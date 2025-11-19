package com.watchfav.api.dto.season;

import com.watchfav.api.dto.episode.GetEpisodeDTO;
import com.watchfav.api.model.Season;

import java.util.List;

public record GetSeasonDetailsDTO(
        Long id,
        Integer number,
        Integer releaseYear,
        String series,
        List<GetEpisodeDTO> episodes
) {
    public GetSeasonDetailsDTO(Season season){
        this(
                season.getId(),
                season.getNumber(),
                season.getReleaseYear(),
                season.getSeries().getName(),
                season.getEpisodes().stream().map(GetEpisodeDTO::new).toList()
        );
    }
}
