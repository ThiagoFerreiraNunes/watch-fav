package com.watchfav.api.model;

import com.watchfav.api.dto.episode.PostEpisodeDTO;
import com.watchfav.api.dto.episode.PutEpisodeDTO;
import jakarta.persistence.*;

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

@Table(name = "tb_episodes")
@Entity(name = "Episode")
public class Episode {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "episode_id")
    private Long id;

    @Column(name = "episode_number")
    private Integer number;

    @Column(name = "episode_name")
    private String name;

    private LocalTime duration;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "season_id")
    private Season season;

    @ManyToMany
    @JoinTable(
            name = "tb_episodes_actors",
            joinColumns = @JoinColumn(name = "episode_id"),
            inverseJoinColumns = @JoinColumn(name = "actor_id")
    )
    private List<Actor> mainActors = new ArrayList<>();

    @ManyToMany
    @JoinTable(
            name = "tb_episodes_directors",
            joinColumns = @JoinColumn(name = "episode_id"),
            inverseJoinColumns = @JoinColumn(name = "director_id")
    )
    private List<Director> directors = new ArrayList<>();

    @Column(name = "is_available")
    private Boolean isAvailable;

    public Episode(){}

    public Episode(PostEpisodeDTO data, Season season, List<Actor> actors, List<Director> directors){
        this.number = data.number();
        this.name = data.name();
        this.duration = data.duration();
        this.season = season;
        this.mainActors = actors;
        this.directors = directors;
    }

    public Long getId() {
        return id;
    }

    public Integer getNumber() {
        return number;
    }

    public String getName() {
        return name;
    }

    public LocalTime getDuration() {
        return duration;
    }

    public Season getSeason() {
        return season;
    }

    public List<Actor> getActors() {
        return mainActors;
    }

    public List<Director> getDirectors() {
        return directors;
    }

    public Boolean getIsAvailable() {
        return isAvailable;
    }

    public void updateData(PutEpisodeDTO data, List<Actor> actors, List<Director> directors) {
        if(data.number() != null) this.number = data.number();
        if(data.name() != null) this.name = data.name();
        if(data.duration() != null) this.duration = data.duration();
        if(actors != null) this.mainActors = actors;
        if(directors != null) this.directors = directors;
    }

    public void delete() {
        isAvailable = false;
    }

    public void reactivate() {
        isAvailable = true;
    }

}