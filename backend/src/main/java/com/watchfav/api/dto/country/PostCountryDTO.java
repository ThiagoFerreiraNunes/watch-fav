package com.watchfav.api.dto.country;

import jakarta.validation.constraints.NotBlank;

public record PostCountryDTO(@NotBlank String name) {
}
