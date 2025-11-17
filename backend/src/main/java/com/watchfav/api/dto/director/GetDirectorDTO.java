package com.watchfav.api.dto.director;

import com.watchfav.api.model.Director;

public record GetDirectorDTO(
        Long id,
        String name,
        String country
) {
    public GetDirectorDTO(Director director){
        this(
                director.getId(),
                director.getName(),
                director.getCountry().getName()
        );
    }
}
