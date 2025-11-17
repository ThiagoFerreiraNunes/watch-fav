package com.watchfav.api.model;

import com.watchfav.api.dto.genre.PostGenreDTO;
import com.watchfav.api.dto.genre.PutGenreDTO;
import com.watchfav.api.model.common.HasAvailability;

public class Genre implements HasAvailability {
    private Long id;
    private String name;
    private Boolean isAvailable;

    public Genre() {}

    public Genre(PostGenreDTO data) {
        this.name = data.name();
        this.isAvailable = true;
    }

    public Genre(Long id, String name, Boolean isAvailable) {
        this.id = id;
        this.name = name;
        this.isAvailable = isAvailable;
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

    @Override
    public Boolean getIsAvailable() {
        return isAvailable;
    }

    public void setIsAvailable(Boolean isAvailable) {
        this.isAvailable = isAvailable;
    }

    public void updateData(PutGenreDTO data) {
        if (data.name() != null) name = data.name();
    }

    public void delete() {
        isAvailable = false;
    }

    public void reactivate() {
        isAvailable = true;
    }
}