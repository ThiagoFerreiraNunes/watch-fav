package com.watchfav.api.model;

import com.watchfav.api.dto.streaming.PostStreamingDTO;
import com.watchfav.api.dto.streaming.PutStreamingDTO;
import com.watchfav.api.model.common.HasAvailability;

public class Streaming implements HasAvailability {

    private Long id;
    private String name;
    private String url;
    private Boolean isAvailable;

    public Streaming() {}

    public Streaming(PostStreamingDTO data) {
        this.name = data.name();
        this.url = data.url();
        this.isAvailable = true;
    }

    public Streaming(Long id, String name, String url, Boolean isAvailable) {
        this.id = id;
        this.name = name;
        this.url = url;
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

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    @Override
    public Boolean getIsAvailable() {
        return isAvailable;
    }

    public void setIsAvailable(Boolean isAvailable) {
        this.isAvailable = isAvailable;
    }

    public void updateData(PutStreamingDTO data) {
        if(data.name() != null) this.name = data.name();
        if(data.url() != null) this.url = data.url();
    }

    public void delete() {
        isAvailable = false;
    }

    public void reactivate() {
        isAvailable = true;
    }
}
