package com.watchfav.api.dto.season;

import com.watchfav.api.dto.episode.GetEpisodeDTO;
import com.watchfav.api.model.Season;

import java.util.List;

public record GetSeasonDTO(
        Long id,
        Integer number,
        Integer releaseYear,
        List<GetEpisodeDTO> episodes
) {
    public GetSeasonDTO(Season season){
        this(
                season.getId(),
                season.getNumber(),
                season.getReleaseYear(),
                season.getEpisodes().stream().map(GetEpisodeDTO::new).toList()
        );
    }
}
