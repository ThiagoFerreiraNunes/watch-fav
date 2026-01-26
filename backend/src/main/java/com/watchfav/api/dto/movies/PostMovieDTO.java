package com.watchfav.api.dto.movies;

import com.watchfav.api.model.AgeRating;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;

import java.time.LocalTime;
import java.util.List;

public record PostMovieDTO(
        @NotBlank String name,
        @NotBlank String imageUrl,
        @NotBlank String description,
        @NotNull Integer releaseYear,
        @NotNull LocalTime duration,
        @NotNull AgeRating ageRating,
        @NotNull Long countryId,
        @NotEmpty List<Long> genreIds,
        @NotEmpty List<Long> mainActorIds,
        @NotEmpty List<Long> directorIds,
        @NotEmpty List<Long> languageIds,
        @NotEmpty List<Long> streamingIds
){
}
