package com.watchfav.api.dto.movies;

import com.watchfav.api.dto.actor.GetActorDTO;
import com.watchfav.api.dto.country.GetCountryDTO;
import com.watchfav.api.dto.director.GetDirectorDTO;
import com.watchfav.api.dto.genre.GetGenreDTO;
import com.watchfav.api.dto.language.GetLanguageDTO;
import com.watchfav.api.dto.streaming.GetStreamingDTO;
import com.watchfav.api.model.Country;
import com.watchfav.api.model.Movie;

import java.time.LocalTime;
import java.util.List;

public record GetMovieDetailsDTO(
        Long id,
        String name,
        String imageUrl,
        String description,
        Integer releaseYear,
        LocalTime duration,
        String ageRating,
        GetCountryDTO country,
        List<GetGenreDTO> genres,
        List<GetActorDTO> mainActors,
        List<GetDirectorDTO> directors,
        List<GetLanguageDTO> languages,
        List<GetStreamingDTO> streamings
) {
    public GetMovieDetailsDTO(Movie movie){
        this(
                movie.getId(),
                movie.getName(),
                movie.getImageUrl(),
                movie.getDescription(),
                movie.getReleaseYear(),
                movie.getDuration(),
                movie.getAgeRating().getName(),
                new GetCountryDTO(movie.getCountry()),
                movie.getGenres().stream().map(GetGenreDTO::new).toList(),
                movie.getMainActors().stream().map(GetActorDTO::new).toList(),
                movie.getDirectors().stream().map(GetDirectorDTO::new).toList(),
                movie.getLanguages().stream().map(GetLanguageDTO::new).toList(),
                movie.getStreamings().stream().map(GetStreamingDTO::new).toList()
        );
    }
}
