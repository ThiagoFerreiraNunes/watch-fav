package com.watchfav.api.dto.director;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

public record PostDirectorDTO(
        @NotBlank String name,
        @NotNull Long countryId
) {
}