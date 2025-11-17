package com.watchfav.api.dto.actor;

import com.watchfav.api.model.Actor;

public record GetActorDTO(
        Long id,
        String name,
        String country
) {
    public GetActorDTO(Actor actor){
        this(
                actor.getId(),
                actor.getName(),
                actor.getCountry().getName()
        );
    }
}
