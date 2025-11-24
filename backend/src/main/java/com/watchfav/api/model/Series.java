package com.watchfav.api.model;

import com.watchfav.api.dto.series.PostSeriesDTO;
import com.watchfav.api.dto.series.PutSeriesDTO;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;

@Table(name = "tb_series")
@Entity(name = "Series")
public class Series {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "series_id")
    private Long id;

    @Column(name = "series_name")
    private String name;

    @Column(name = "image_url")
    private String imageUrl;

    @Column(name = "series_description")
    private String description;

    @Column(name = "release_year")
    private Integer releaseYear;

    @Enumerated(EnumType.STRING)
    @Column(name = "age_rating")
    private AgeRating ageRating;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "country_id")
    private Country country;

    @ManyToMany
    @JoinTable(
            name = "tb_series_streamings",
            joinColumns = @JoinColumn(name = "series_id"),
            inverseJoinColumns = @JoinColumn(name = "streaming_id")
    )
    private List<Streaming> streamings = new ArrayList<>();

    @ManyToMany
    @JoinTable(
            name = "tb_series_genres",
            joinColumns = @JoinColumn(name = "series_id"),
            inverseJoinColumns = @JoinColumn(name = "genre_id")
    )
    private List<Genre> genres = new ArrayList<>();

    @ManyToMany
    @JoinTable(
            name = "tb_series_languages",
            joinColumns = @JoinColumn(name = "series_id"),
            inverseJoinColumns = @JoinColumn(name = "language_id")
    )
    private List<Language> languages = new ArrayList<>();

    @OneToMany(mappedBy = "series")
    private List<Season> seasons = new ArrayList<>();

    @Column(name = "is_available")
    private Boolean isAvailable;

    public Series(){}

    public Series(PostSeriesDTO data, Country country, List<Genre> genres, List<Language> languages) {
        this.name = data.name();
        this.imageUrl = data.imageUrl();
        this.description = data.description();
        this.releaseYear = data.releaseYear();
        this.ageRating = data.ageRating();
        this.country = country;
        this.genres = genres;
        this.languages = languages;
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

    public AgeRating getAgeRating() {
        return ageRating;
    }

    public Country getCountry() {
        return country;
    }

    public List<Season> getSeasons() {
        return seasons;
    }

    public List<Genre> getGenres() {
        return genres;
    }

    public List<Language> getLanguages() {
        return languages;
    }

    public List<Streaming> getStreamings() {
        return streamings;
    }

    public Boolean getIsAvailable() {
        return isAvailable;
    }

    public void updateData(PutSeriesDTO data, Country country, List<Genre> genres, List<Language> languages) {
        if(data.name() != null) this.name = data.name();
        if(data.imageUrl() != null) this.imageUrl = data.imageUrl();
        if(data.description() != null) this.description = data.description();
        if(data.releaseYear() != null) this.releaseYear = data.releaseYear();
        if(data.ageRating() != null) this.ageRating = data.ageRating();
        if(data.countryId() != null) this.country = country;
        if(data.genreIds() != null) this.genres = genres;
        if(data.languageIds() != null) this.languages = languages;
    }

    public void delete() {
        isAvailable = false;
    }

    public void reactivate() {
        isAvailable = true;
    }

}
