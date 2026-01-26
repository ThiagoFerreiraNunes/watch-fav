package com.watchfav.api.dto.series;

import com.watchfav.api.dto.country.GetCountryDTO;
import com.watchfav.api.dto.genre.GetGenreDTO;
import com.watchfav.api.dto.language.GetLanguageDTO;
import com.watchfav.api.dto.season.GetSeasonDTO;
import com.watchfav.api.dto.streaming.GetStreamingDTO;
import com.watchfav.api.model.Series;

import java.util.List;

public record GetSeriesDetailsDTO(
        Long id,
        String name,
        String imageUrl,
        String description,
        Integer releaseYear,
        String ageRating,
        GetCountryDTO country,
        List<GetGenreDTO> genres,
        List<GetLanguageDTO> languages,
        List<GetSeasonDTO> seasons,
        List<GetStreamingDTO> streamings

) {
    public GetSeriesDetailsDTO(Series series){
        this(
                series.getId(),
                series.getName(),
                series.getImageUrl(),
                series.getDescription(),
                series.getReleaseYear(),
                series.getAgeRating().getName(),
                new GetCountryDTO(series.getCountry()),
                series.getGenres().stream().map(GetGenreDTO::new).toList(),
                series.getLanguages().stream().map(GetLanguageDTO::new).toList(),
                series.getSeasons().stream().map(GetSeasonDTO::new).toList(),
                series.getStreamings().stream().map(GetStreamingDTO::new).toList()
        );
    }
}
