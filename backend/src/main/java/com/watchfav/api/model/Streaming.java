package com.watchfav.api.model;

import com.watchfav.api.dto.streaming.PostStreamingDTO;
import com.watchfav.api.dto.streaming.PutStreamingDTO;
import com.watchfav.api.model.common.HasAvailability;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;

@Table(name = "tb_streamings")
@Entity(name = "Streaming")
public class Streaming implements HasAvailability{

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "streaming_id")
    private Long id;

    @Column(name = "streaming_name")
    private String name;

    @Column(name = "streaming_url")
    private String url;

    @ManyToMany(mappedBy = "streamings")
    private List<Movie> movies = new ArrayList<>();

    @Column(name = "is_available")
    private Boolean isAvailable;

    public Streaming(){}

    public Streaming(PostStreamingDTO data) {
        this.name = data.name();
        this.url = data.url();
        this.isAvailable = true;
    }


    public Long getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public String getUrl() {
        return url;
    }

    public List<Movie> getMovies() {
        return movies;
    }

    @Override
    public Boolean getIsAvailable() {
        return isAvailable;
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
