package com.watchfav.api.dto.series;

import com.watchfav.api.model.Series;

public record GetSeriesDTO(
        Long id,
        String name,
        String imageUrl,
        Integer releaseYear,
        Integer seasons,
        String ageRating
) {
    public GetSeriesDTO(Series series){
        this(
                series.getId(),
                series.getName(),
                series.getImageUrl(),
                series.getReleaseYear(),
                series.getSeasons().size(),
                series.getAgeRating().getName());
    }
}
