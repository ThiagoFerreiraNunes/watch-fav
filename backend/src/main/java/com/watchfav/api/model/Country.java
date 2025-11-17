package com.watchfav.api.model;

import com.watchfav.api.dto.country.PostCountryDTO;
import com.watchfav.api.dto.country.PutCountryDTO;
import com.watchfav.api.model.common.HasAvailability;

public class Country implements HasAvailability {
    private Long id;
    private String name;
    private Boolean isAvailable;

    public Country(){}

    public Country(PostCountryDTO data){
        this.name = data.name();
        this.isAvailable = true;
    }

    public Country(Long id, String name, Boolean isAvailable) {
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


    public void updateData(PutCountryDTO data){
        if(data.name() != null) name = data.name();
    }

    public void delete() {
        isAvailable = false;
    }

    public void reactivate() {
        isAvailable = true;
    }
}
