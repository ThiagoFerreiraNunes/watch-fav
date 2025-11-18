package com.watchfav.api.service.movie;

import com.watchfav.api.dto.movies.GetMovieDTO;
import com.watchfav.api.dto.movies.GetMovieDetailsDTO;
import com.watchfav.api.dto.movies.PostMovieDTO;
import com.watchfav.api.dto.movies.PutMovieDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.model.*;
import com.watchfav.api.repository.*;
import com.watchfav.api.service.commonValidation.EntitiesValidator;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.ArrayList;
import java.util.List;

@Service
public class MovieService {

    @Autowired private MovieRepository movieRepository;
    @Autowired private CountryRepository countryRepository;
    @Autowired private GenreRepository genreRepository;
    @Autowired private ActorRepository actorRepository;
    @Autowired private DirectorRepository directorRepository;
    @Autowired private LanguageRepository languageRepository;
    @Autowired private StreamingRepository streamingRepository;
    @Autowired private EntitiesValidator entitiesValidator;

    @Transactional
    public GetMovieDetailsDTO postAMovie(@RequestBody @Valid PostMovieDTO data){
        Country country = countryRepository.findById(data.countryId())
                .orElseThrow(() -> new EntityNotFoundException("Country not found."));

        if(Boolean.FALSE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is deleted.");
        }

        List<Genre> genres = genreRepository.findAllById(data.genreIds());
        entitiesValidator.validate(genres, data.genreIds(), "Genre");

        List<Actor> actors = actorRepository.findAllById(data.mainActorIds());
        entitiesValidator.validate(actors, data.mainActorIds(), "Actor");

        List<Director> directors = directorRepository.findAllById(data.directorIds());
        entitiesValidator.validate(directors, data.directorIds(), "Director");

        List<Language> languages = languageRepository.findAllById(data.languageIds());
        entitiesValidator.validate(languages, data.languageIds(), "Language");

        List<Streaming> streamings = streamingRepository.findAllById(data.streamingIds());
        entitiesValidator.validate(streamings, data.streamingIds(), "Streaming");

        Movie movie = new Movie(data, country, genres, actors, directors, languages, streamings);

        movieRepository.save(movie);

        return new GetMovieDetailsDTO(movie);

    }

    public Page<GetMovieDTO> getAllMovies(Pageable pageable){
        return movieRepository.findAllByAvailable(pageable).map(GetMovieDTO::new);
    }

    public GetMovieDetailsDTO getAMovie(Long id){
        Movie movie = movieRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Movie not found."));

        if(Boolean.FALSE.equals(movie.getIsAvailable())){
            throw new BusinessRuleException("Movie is deleted.");
        }

        return new GetMovieDetailsDTO(movie);
    }

    @Transactional
    public GetMovieDetailsDTO putAMovie(Long id, PutMovieDTO data){
        Movie movie = movieRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Movie not found."));

        if(Boolean.FALSE.equals(movie.getIsAvailable())){
            throw new BusinessRuleException("Movie is deleted.");
        }

        Country country = null;
        List<Genre> genres = new ArrayList<>();
        List<Actor> actors = new ArrayList<>();
        List<Director> directors = new ArrayList<>();
        List<Language> languages = new ArrayList<>();
        List<Streaming> streamings = new ArrayList<>();

        if(data.countryId() != null){
            country = countryRepository.findById(data.countryId())
                    .orElseThrow(() -> new EntityNotFoundException("Country not found."));

            if(Boolean.FALSE.equals(country.getIsAvailable())){
                throw new BusinessRuleException("Country is deleted.");
            }
        }

        if(data.genreIds() != null) {
            genres = genreRepository.findAllById(data.genreIds());
            entitiesValidator.validate(genres, data.genreIds(), "Genre");
        }
        if(data.mainActorIds() != null) {
            actors = actorRepository.findAllById(data.mainActorIds());
            entitiesValidator.validate(actors, data.mainActorIds(), "Actor");
        }
        if(data.directorIds() != null) {
            directors = directorRepository.findAllById(data.directorIds());
            entitiesValidator.validate(directors, data.directorIds(), "Director");
        }
        if(data.languageIds() != null) {
            languages = languageRepository.findAllById(data.languageIds());
            entitiesValidator.validate(languages, data.languageIds(), "Language");
        }
        if(data.streamingIds() != null) {
            streamings = streamingRepository.findAllById(data.streamingIds());
            entitiesValidator.validate(streamings, data.streamingIds(), "Streaming");
        }

        movie.updateData(data, country, genres, actors, directors, languages, streamings);

        return new GetMovieDetailsDTO(movie);
    }

    @Transactional
    public void deleteAMovie(Long id){
        Movie movie = movieRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Movie not found."));

        if(Boolean.FALSE.equals(movie.getIsAvailable())){
            throw new BusinessRuleException("Movie is already deleted.");
        }

        movie.delete();
    }

    @Transactional
    public GetMovieDetailsDTO reactivateAMovie(Long id){
        Movie movie = movieRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Movie not found."));

        if(Boolean.TRUE.equals(movie.getIsAvailable())){
            throw new BusinessRuleException("Movie is already active.");
        }

        movie.reactivate();

        return new GetMovieDetailsDTO(movie);
    }

}

