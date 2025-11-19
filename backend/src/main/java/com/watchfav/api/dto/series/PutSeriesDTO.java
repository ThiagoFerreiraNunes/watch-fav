package com.watchfav.api.dto.series;

import com.watchfav.api.model.AgeRating;

import java.util.List;

public record PutSeriesDTO(
        String name,
        String imageUrl,
        String description,
        Integer releaseYear,
        AgeRating ageRating,
        Long countryId,
        List<Long> genreIds,
        List<Long> languageIds
) {
}
