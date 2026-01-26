package com.watchfav.api.dto.country;

import com.watchfav.api.model.Country;

public record GetCountryDTO(
        Long id,
        String name
) {
    public GetCountryDTO(Country country){
        this(
                country.getId(),
                country.getName()
        );
    }
}
