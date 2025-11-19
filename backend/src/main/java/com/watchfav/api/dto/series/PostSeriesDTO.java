package com.watchfav.api.dto.series;

import com.watchfav.api.model.AgeRating;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;

import java.util.List;

public record PostSeriesDTO(
        @NotBlank String name,
        @NotBlank String imageUrl,
        @NotBlank String description,
        @NotNull Integer releaseYear,
        @NotNull AgeRating ageRating,
        @NotNull Long countryId,
        @NotEmpty List<Long> genreIds,
        @NotEmpty List<Long> languageIds
) {
}
