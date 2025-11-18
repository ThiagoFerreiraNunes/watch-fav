package com.watchfav.api.model;

import com.watchfav.api.dto.actor.PostActorDTO;
import com.watchfav.api.dto.actor.PutActorDTO;
import com.watchfav.api.model.common.HasAvailability;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;

@Table(name = "tb_actors")
@Entity(name = "Actor")
public class Actor implements HasAvailability {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "actor_id")
    private Long id;

    @Column(name = "actor_name")
    private String name;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "country_id")
    private Country country;

    @ManyToMany(mappedBy = "mainActors")
    private List<Movie> movies = new ArrayList<>();

    @Column(name = "is_available")
    private Boolean isAvailable;

    public Actor(){}

    public Actor(PostActorDTO data, Country country){
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

    public void updateData(PutActorDTO data, Country country){
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
