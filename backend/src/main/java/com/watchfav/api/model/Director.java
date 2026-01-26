package com.watchfav.api.model;

import com.watchfav.api.dto.director.PostDirectorDTO;
import com.watchfav.api.dto.director.PutDirectorDTO;
import com.watchfav.api.model.common.HasAvailability;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;

@Table(name = "tb_directors")
@Entity(name = "Director")
public class Director implements HasAvailability {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "director_id")
    private Long id;

    @Column(name = "director_name")
    private String name;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "country_id")
    private Country country;

    @ManyToMany(mappedBy = "directors")
    private List<Movie> movies = new ArrayList<>();

    @Column(name = "is_available")
    private Boolean isAvailable;

    public Director(){}

    public Director(PostDirectorDTO data, Country country){
        this.name = data.name();
        this.country = country;
        this.isAvailable = true;
    }

    public Long getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public Country getCountry() {
        return country;
    }

    public List<Movie> getMovies() {
        return movies;
    }

    @Override
    public Boolean getIsAvailable() {
        return isAvailable;
    }

    public void updateData(PutDirectorDTO data, Country country){
        if(data.name() != null) name = data.name();
        if(country != null) this.country = country;
    }

    public void delete() {
        isAvailable = false;
    }

    public void reactivate() {
        isAvailable = true;
    }
}

