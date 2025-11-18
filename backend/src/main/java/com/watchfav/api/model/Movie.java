package com.watchfav.api.model;

import com.watchfav.api.dto.movies.PostMovieDTO;
import com.watchfav.api.dto.movies.PutMovieDTO;
import jakarta.persistence.*;

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

@Table(name = "tb_movies")
@Entity(name = "Movie")
public class Movie {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "movie_id")
    private Long id;

    @Column(name = "image_url")
    private String imageUrl;

    @Column(name = "movie_name")
    private String name;

    @Column(name = "movie_description")
    private String description;

    @Column(name = "release_year")
    private Integer releaseYear;

    private LocalTime duration;

    @Enumerated(EnumType.STRING)
    @Column(name = "age_rating")
    private AgeRating ageRating;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "country_id")
    private Country country;

    @ManyToMany
    @JoinTable(
            name = "tb_movies_genres",
            joinColumns = @JoinColumn(name = "movie_id"),
            inverseJoinColumns = @JoinColumn(name = "genre_id")
    )
    private List<Genre> genres = new ArrayList<>();

    @ManyToMany
    @JoinTable(
            name = "tb_movies_streamings",
            joinColumns = @JoinColumn(name = "movie_id"),
            inverseJoinColumns = @JoinColumn(name = "streaming_id")
    )
    private List<Streaming> streamings = new ArrayList<>();

    @ManyToMany
    @JoinTable(
            name = "tb_movies_languages",
            joinColumns = @JoinColumn(name = "movie_id"),
            inverseJoinColumns = @JoinColumn(name = "language_id")
    )
    private List<Language> languages = new ArrayList<>();

    @ManyToMany
    @JoinTable(
            name = "tb_movies_actors",
            joinColumns = @JoinColumn(name = "movie_id"),
            inverseJoinColumns = @JoinColumn(name = "actor_id")
    )
    private List<Actor> mainActors = new ArrayList<>();

    @ManyToMany
    @JoinTable(
            name = "tb_movies_directors",
            joinColumns = @JoinColumn(name = "movie_id"),
            inverseJoinColumns = @JoinColumn(name = "director_id")
    )
    private List<Director> directors = new ArrayList<>();

    @Column(name = "is_available")
    private Boolean isAvailable;

    public Movie(){}

    public Movie(PostMovieDTO data,
                 Country country,
                 List<Genre> genres,
                 List<Actor> actors,
                 List<Director> directors,
                 List<Language> languages,
                 List<Streaming> streamings){
        this.name = data.name();
        this.imageUrl = data.imageUrl();
        this.description = data.description();
        this.releaseYear = data.releaseYear();
        this.duration = data.duration();
        this.ageRating = data.ageRating();
        this.country = country;
        this.genres = genres;
        this.mainActors = actors;
        this.directors = directors;
        this.languages = languages;
        this.streamings = streamings;
        this.isAvailable = true;
    }

    public Long getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public String getImageUrl() {
        return imageUrl;
    }

    public String getDescription() {
        return description;
    }

    public Integer getReleaseYear() {
        return releaseYear;
    }

    public LocalTime getDuration() {
        return duration;
    }

    public AgeRating getAgeRating() {
        return ageRating;
    }

    public Country getCountry() {
        return country;
    }

    public List<Genre> getGenres() {
        return genres;
    }

    public List<Streaming> getStreamings() {
        return streamings;
    }

    public List<Language> getLanguages() {
        return languages;
    }

    public List<Director> getDirectors() {
        return directors;
    }

    public List<Actor> getMainActors() {
        return mainActors;
    }

    public Boolean getIsAvailable() {
        return isAvailable;
    }

    public void updateData(PutMovieDTO data, Country country, List<Genre> genres, List<Actor> actors, List<Director> directors, List<Language> languages, List<Streaming> streamings) {
        if(data.name() != null) this.name = data.name();
        if(data.imageUrl() != null) this.imageUrl = data.imageUrl();
        if(data.description() != null) this.description = data.description();
        if(data.releaseYear() != null) this.releaseYear = data.releaseYear();
        if(data.duration() != null) this.duration = data.duration();
        if(data.ageRating() != null) this.ageRating = data.ageRating();
        if(country != null) this.country = country;
        if (genres != null) this.genres = genres;
        if (actors != null) this.mainActors = actors;
        if (directors != null) this.directors = directors;
        if (languages != null) this.languages = languages;
        if(streamings != null) this.streamings = streamings;
    }

    public void delete() {
        isAvailable = false;
    }

    public void reactivate() {
        isAvailable = true;
    }
}