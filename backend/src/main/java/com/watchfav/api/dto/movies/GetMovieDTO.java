package com.watchfav.api.dto.movies;

import com.watchfav.api.model.Movie;

import java.time.LocalTime;

public record GetMovieDTO(
        Long id,
        String name,
        String imageUrl,
        Integer releaseYear,
        LocalTime duration,
        String ageRating
) {
    public GetMovieDTO(Movie movie){
        this(
                movie.getId(),
                movie.getName(),
                movie.getImageUrl(),
                movie.getReleaseYear(),
                movie.getDuration(),
                movie.getAgeRating().getName()
        );
    }
}
