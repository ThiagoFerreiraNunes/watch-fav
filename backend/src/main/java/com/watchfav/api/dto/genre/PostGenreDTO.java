package com.watchfav.api.dto.genre;

import jakarta.validation.constraints.NotBlank;

public record PostGenreDTO(@NotBlank String name
){}
