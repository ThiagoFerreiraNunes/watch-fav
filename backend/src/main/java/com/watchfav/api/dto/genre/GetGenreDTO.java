package com.watchfav.api.dto.genre;

import com.watchfav.api.model.Genre;

public record GetGenreDTO(
        Long id,
        String name
) {
    public GetGenreDTO(Genre genre){
        this(
                genre.getId(),
                genre.getName());
    }
}
