package com.watchfav.api.dto.season;

import jakarta.validation.constraints.NotNull;

public record PostSeasonDTO(
        @NotNull Integer number,
        @NotNull Integer releaseYear
) {
}
