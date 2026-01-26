package com.watchfav.api.dto.language;

import jakarta.validation.constraints.NotBlank;

public record PostLanguageDTO(@NotBlank String name) {
}

