package com.watchfav.api.dto.episode;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;

import java.time.LocalTime;
import java.util.List;

public record PostEpisodeDTO(
        @NotNull Integer number,
        @NotBlank String name,
        @NotNull LocalTime duration,
        @NotEmpty List<Long> mainActorIds,
        @NotEmpty List<Long> directorIds
) {
}
