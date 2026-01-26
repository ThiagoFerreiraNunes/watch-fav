package com.watchfav.api.model;

import com.watchfav.api.dto.country.PostCountryDTO;
import com.watchfav.api.dto.country.PutCountryDTO;
import com.watchfav.api.model.common.HasAvailability;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;

@Table(name = "tb_countries")
@Entity(name = "Country")
public class Country implements HasAvailability {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "country_id")
    private Long id;

    @Column(name = "country_name")
    private String name;

    @Column(name = "is_available")
    private Boolean isAvailable;

    @OneToMany(mappedBy = "country")
    private List<Movie> movies = new ArrayList<>();

    @OneToMany(mappedBy = "country")
    private List<Actor> actors = new ArrayList<>();

    public Country(){}

    public Country(PostCountryDTO data){
        this.name = data.name();
        this.isAvailable = true;
    }

    public Long getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    @Override
    public Boolean getIsAvailable() {
        return isAvailable;
    }

    public List<Movie> getMovies() {
        return movies;
    }

    public List<Actor> getActors() {
        return actors;
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
