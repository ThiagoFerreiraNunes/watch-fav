package com.watchfav.api.model;

import com.watchfav.api.dto.genre.PostGenreDTO;
import com.watchfav.api.dto.genre.PutGenreDTO;
import com.watchfav.api.model.common.HasAvailability;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;

@Table(name = "tb_genres")
@Entity(name = "Genre")
public class Genre implements HasAvailability {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "genre_id")
    private Long id;

    @Column(name = "genre_name")
    private String name;

    @ManyToMany(mappedBy = "genres")
    private List<Movie> movies = new ArrayList<>();

    @Column(name = "is_available")
    private Boolean isAvailable;

    public Genre(){}

    public Genre(PostGenreDTO data){
        this.name = data.name();
        this.isAvailable = true;
    }

    public Long getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public List<Movie> getMovies() {
        return movies;
    }

    @Override
    public Boolean getIsAvailable() {
        return isAvailable;
    }

    public void updateData(PutGenreDTO data){
        if(data.name() != null) name = data.name();
    }

    public void delete() {
        isAvailable = false;
    }

    public void reactivate() {
        isAvailable = true;
    }
}