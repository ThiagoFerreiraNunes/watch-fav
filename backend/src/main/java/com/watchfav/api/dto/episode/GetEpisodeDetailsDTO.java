package com.watchfav.api.dto.episode;

import com.watchfav.api.dto.actor.GetActorDTO;
import com.watchfav.api.dto.director.GetDirectorDTO;
import com.watchfav.api.model.Episode;

import java.time.LocalTime;
import java.util.List;

public record GetEpisodeDetailsDTO(
        Long id,
        Integer number,
        String name,
        LocalTime duration,
        String series,
        Integer season,
        List<GetActorDTO> mainActors,
        List<GetDirectorDTO> directors
) {
    public GetEpisodeDetailsDTO(Episode episode){
        this(
                episode.getId(),
                episode.getNumber(),
                episode.getName(),
                episode.getDuration(),
                episode.getSeason().getSeries().getName(),
                episode.getSeason().getNumber(),
                episode.getActors().stream().map(GetActorDTO::new).toList(),
                episode.getDirectors().stream().map(GetDirectorDTO::new).toList()
        );
    }
}
