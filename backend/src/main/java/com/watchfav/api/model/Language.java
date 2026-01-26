package com.watchfav.api.model;

import com.watchfav.api.dto.language.PostLanguageDTO;
import com.watchfav.api.dto.language.PutLanguageDTO;
import com.watchfav.api.model.common.HasAvailability;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;

@Table(name = "tb_languages")
@Entity(name = "Language")
public class Language implements HasAvailability {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "language_id")
    private Long id;

    @Column(name = "language_name")
    private String name;

    @ManyToMany(mappedBy = "languages")
    private List<Movie> movies = new ArrayList<>();

    @Column(name = "is_available")
    private Boolean isAvailable;

    public Language(){}

    public Language(PostLanguageDTO data){
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

    public void updateData(PutLanguageDTO data){
        if(data.name() != null) name = data.name();
    }

    public void delete() {
        isAvailable = false;
    }

    public void reactivate() {
        isAvailable = true;
    }
}