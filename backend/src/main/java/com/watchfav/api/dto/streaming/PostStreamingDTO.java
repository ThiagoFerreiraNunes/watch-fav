package com.watchfav.api.dto.streaming;

import jakarta.validation.constraints.NotBlank;

public record PostStreamingDTO(
        @NotBlank String name,
        @NotBlank String url
) {
}
