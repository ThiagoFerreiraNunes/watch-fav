package com.watchfav.api.model;

import com.watchfav.api.dto.language.PostLanguageDTO;
import com.watchfav.api.dto.language.PutLanguageDTO;
import com.watchfav.api.model.common.HasAvailability;

public class Language implements HasAvailability {
    private Long id;
    private String name;
    private Boolean isAvailable;

    public Language() {}

    public Language(PostLanguageDTO data) {
        this.name = data.name();
        this.isAvailable = true;
    }

    public Language(Long id, String name, Boolean isAvailable) {
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

    public void updateData(PutLanguageDTO data) {
        if (data.name() != null) name = data.name();
    }

    public void delete() {
        isAvailable = false;
    }

    public void reactivate() {
        isAvailable = true;
    }
}