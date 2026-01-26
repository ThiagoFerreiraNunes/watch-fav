package com.watchfav.api.dto.movies;

import com.watchfav.api.model.AgeRating;

import java.time.LocalTime;
import java.util.List;

public record PutMovieDTO(
        String name,
        String imageUrl,
        String description,
        Integer releaseYear,
        LocalTime duration,
        AgeRating ageRating,
        Long countryId,
        List<Long> genreIds,
        List<Long> mainActorIds,
        List<Long> directorIds,
        List<Long> languageIds,
        List<Long> streamingIds
) {
}
