package com.watchfav.api.dto.actor;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

public record PostActorDTO(
        @NotBlank String name,
        @NotNull Long countryId
) {
}