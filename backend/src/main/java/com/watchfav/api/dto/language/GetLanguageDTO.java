package com.watchfav.api.dto.language;

import com.watchfav.api.model.Language;

public record GetLanguageDTO(
        Long id,
        String name) {
    public GetLanguageDTO(Language language){
        this(
                language.getId(),
                language.getName()
        );
    }
}
