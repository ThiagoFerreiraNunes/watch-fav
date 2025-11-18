package com.watchfav.api.model;

import com.watchfav.api.dto.movies.PostMovieDTO;
import com.watchfav.api.dto.movies.PutMovieDTO;

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

public class Movie {

    private Long id;
    private String imageUrl;
    private String name;
    private String description;
    private Integer releaseYear;
    private LocalTime duration;
    private AgeRating ageRating;
    private Country country; // ManyToOne
    private Boolean isAvailable;

    private List<Genre> genres = new ArrayList<>();
    private List<Streaming> streamings = new ArrayList<>();
    private List<Language> languages = new ArrayList<>();
    private List<Actor> mainActors = new ArrayList<>();
    private List<Director> directors = new ArrayList<>();

    public Movie(){}

    public Movie(Long id, String name, String imageUrl, String description, Integer releaseYear, LocalTime duration, AgeRating ageRating, Country country, Boolean isAvailable) {
        this.id = id;
        this.name = name;
        this.imageUrl = imageUrl;
        this.description = description;
        this.releaseYear = releaseYear;
        this.duration = duration;
        this.ageRating = ageRating;
        this.country = country;
        this.isAvailable = isAvailable;
    }

    public Movie(PostMovieDTO data, Country country) {
        this.name = data.name();
        this.description = data.description();
        this.imageUrl = data.imageUrl();
        this.releaseYear = data.releaseYear();
        this.duration = data.duration();
        this.ageRating = data.ageRating();
        this.country = country;
        this.isAvailable = true;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getImageUrl() {
        return imageUrl;
    }

    public void setImageUrl(String imageUrl) {
        this.imageUrl = imageUrl;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Integer getReleaseYear() {
        return releaseYear;
    }

    public void setReleaseYear(Integer releaseYear) {
        this.releaseYear = releaseYear;
    }

    public LocalTime getDuration() {
        return duration;
    }

    public void setDuration(LocalTime duration) {
        this.duration = duration;
    }

    public AgeRating getAgeRating() {
        return ageRating;
    }

    public void setAgeRating(AgeRating ageRating) {
        this.ageRating = ageRating;
    }

    public Country getCountry() {
        return country;
    }

    public void setCountry(Country country) {
        this.country = country;
    }

    public Boolean getIsAvailable() {
        return isAvailable;
    }

    public void setAvailable(Boolean available) {
        isAvailable = available;
    }

    public List<Genre> getGenres() {
        return genres;
    }

    public void setGenres(List<Genre> genres) {
        this.genres = genres;
    }

    public List<Streaming> getStreamings() {
        return streamings;
    }

    public void setStreamings(List<Streaming> streamings) {
        this.streamings = streamings;
    }

    public List<Language> getLanguages() {
        return languages;
    }

    public void setLanguages(List<Language> languages) {
        this.languages = languages;
    }

    public List<Actor> getMainActors() {
        return mainActors;
    }

    public void setMainActors(List<Actor> mainActors) {
        this.mainActors = mainActors;
    }

    public List<Director> getDirectors() {
        return directors;
    }

    public void setDirectors(List<Director> directors) {
        this.directors = directors;
    }

    public void updateData(PutMovieDTO data, Country country, List<Genre> genres, List<Actor> actors, List<Director> directors, List<Language> languages, List<Streaming> streamings) {
        if(data.name() != null) name = data.name();
        if(data.description() != null) description = data.description();
        if(data.releaseYear() != null) releaseYear = data.releaseYear();
        if(data.duration() != null) duration = data.duration();
        if(data.ageRating() != null) ageRating = data.ageRating();
        if(data.imageUrl() != null) imageUrl = data.imageUrl();
        if(country != null) this.country = country;
        if (genres != null) this.genres = genres;
        if (actors != null) this.mainActors = actors;
        if (directors != null) this.directors = directors;
        if (languages != null) this.languages = languages;
        if (streamings != null) this.streamings = streamings;
    }

    public void delete() { isAvailable = false; }
    public void reactivate() { isAvailable = true; }
}
