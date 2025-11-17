package com.watchfav.api.model;

import com.watchfav.api.dto.actor.PostActorDTO;
import com.watchfav.api.dto.actor.PutActorDTO;
import com.watchfav.api.model.common.HasAvailability;

public class Actor implements HasAvailability {

    private Long id;
    private String name;
    private Country country;
    private Boolean isAvailable;

    public Actor() {}

    public Actor(PostActorDTO data, Country country) {
        this.name = data.name();
        this.country = country;
        this.isAvailable = true;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Country getCountry() {
        return country;
    }

    public void setCountry(Country country) {
        this.country = country;
    }

    @Override
    public Boolean getIsAvailable() {
        return isAvailable;
    }

    public void setIsAvailable(Boolean isAvailable) {
        this.isAvailable = isAvailable;
    }

    public void updateData(PutActorDTO data, Country country) {
        if (data.name() != null) name = data.name();
        if (country != null) this.country = country;
    }

    public void delete() {
        isAvailable = false;
    }

    public void reactivate() {
        isAvailable = true;
    }
}
