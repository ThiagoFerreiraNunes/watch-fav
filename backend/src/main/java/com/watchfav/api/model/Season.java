package com.watchfav.api.model;

import com.watchfav.api.dto.season.PostSeasonDTO;
import com.watchfav.api.dto.season.PutSeasonDTO;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;

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

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "series_id")
    private Series series;

    @OneToMany(mappedBy = "season")
    private List<Episode> episodes = new ArrayList<>();

    @Column(name = "is_available")
    private Boolean isAvailable;

    public Season(){}

    public Season(Series series, PostSeasonDTO data) {
        this.number = data.number();
        this.releaseYear = data.releaseYear();
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

    public List<Episode> getEpisodes() {
        return episodes;
    }

    public Boolean getIsAvailable() {
        return isAvailable;
    }

    public void updateData(PutSeasonDTO data) {
        if(data.number() != null) this.number = data.number();
        if(data.releaseYear() != null) this.releaseYear = data.releaseYear();
    }

    public void delete() {
        isAvailable = false;
    }

    public void reactivate() {
        isAvailable = true;
    }

}
