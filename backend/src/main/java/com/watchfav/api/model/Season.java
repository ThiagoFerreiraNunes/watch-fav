package com.watchfav.api.model;

import com.watchfav.api.dto.season.PostSeasonDTO;
import com.watchfav.api.dto.season.PutSeasonDTO;
import jakarta.persistence.*;

@Table(name = "tb_seasons")
@Entity(name = "Season")
public class Season {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "season_id")
    private Long id;

    @Column(name = "season_number")
    private Integer number;

    @Column(name = "release_year")
    private Integer releaseYear;

    @Column(name = "episodes")
    private Integer episodes;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "series_id")
    private Series series;

    @Column(name = "is_available")
    private Boolean isAvailable;

    public Season(){}

    public Season(Series series, PostSeasonDTO data) {
        this.number = data.number();
        this.releaseYear = data.releaseYear();
        this.episodes = data.episodes();
        this.series = series;
        this.isAvailable = true;
    }

    public Long getId() {
        return id;
    }

    public Integer getNumber() {
        return number;
    }

    public Integer getReleaseYear() {
        return releaseYear;
    }

    public Series getSeries() {
        return series;
    }

    public Integer getEpisodes() {
        return episodes;
    }

    public Boolean getIsAvailable() {
        return isAvailable;
    }

    public void updateData(PutSeasonDTO data) {
        if(data.number() != null) this.number = data.number();
        if(data.releaseYear() != null) this.releaseYear = data.releaseYear();
        if(data.episodes() != null) this.episodes = data.episodes();
    }

    public void delete() {
        isAvailable = false;
    }

    public void reactivate() {
        isAvailable = true;
    }

}
