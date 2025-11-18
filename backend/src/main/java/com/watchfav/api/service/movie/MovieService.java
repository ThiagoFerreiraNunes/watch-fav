package com.watchfav.api.service.movie;

import com.watchfav.api.dto.movies.GetMovieDTO;
import com.watchfav.api.dto.movies.GetMovieDetailsDTO;
import com.watchfav.api.dto.movies.PostMovieDTO;
import com.watchfav.api.dto.movies.PutMovieDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.exception.ResourceNotFoundException;
import com.watchfav.api.model.*;
import com.watchfav.api.repository.*;
import com.watchfav.api.service.commonValidation.EntitiesValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class MovieService {

    @Autowired private MovieDAO movieDAO;
    @Autowired private CountryDAO countryDAO;
    @Autowired private GenreDAO genreDAO;
    @Autowired private ActorDAO actorDAO;
    @Autowired private DirectorDAO directorDAO;
    @Autowired private LanguageDAO languageDAO;
    @Autowired private StreamingDAO streamingDAO;

    @Autowired private EntitiesValidator entitiesValidator;

    private void fetchAndSetAllRelationships(Movie movie) {
        Long movieId = movie.getId();

        List<Long> genreIds = movieDAO.findRelatedIds(movieId, "tb_movies_genres", "genre_id");
        movie.setGenres(genreDAO.findAllById(genreIds));

        List<Long> actorIds = movieDAO.findRelatedIds(movieId, "tb_movies_actors", "actor_id");
        movie.setMainActors(actorDAO.findAllById(actorIds));

        List<Long> directorIds = movieDAO.findRelatedIds(movieId, "tb_movies_directors", "director_id");
        movie.setDirectors(directorDAO.findAllById(directorIds));

        List<Long> languageIds = movieDAO.findRelatedIds(movieId, "tb_movies_languages", "language_id");
        movie.setLanguages(languageDAO.findAllById(languageIds));

        List<Long> streamingIds = movieDAO.findRelatedIds(movieId, "tb_movies_streamings", "streaming_id");
        movie.setStreamings(streamingDAO.findAllById(streamingIds));
    }

    private void updateAllRelationships(Long movieId, List<Long> genreIds, List<Long> mainActorIds, List<Long> directorIds, List<Long> languageIds, List<Long> streamingIds) {
        if (genreIds != null) movieDAO.updateRelationships(movieId, "tb_movies_genres", "genre_id", genreIds);
        if (mainActorIds != null) movieDAO.updateRelationships(movieId, "tb_movies_actors", "actor_id", mainActorIds);
        if (directorIds != null) movieDAO.updateRelationships(movieId, "tb_movies_directors", "director_id", directorIds);
        if (languageIds != null) movieDAO.updateRelationships(movieId, "tb_movies_languages", "language_id", languageIds);
        if (streamingIds != null) movieDAO.updateRelationships(movieId, "tb_movies_streamings", "streaming_id", streamingIds);
    }


    @Transactional
    public GetMovieDetailsDTO postAMovie(PostMovieDTO data){
        Country country = countryDAO.findById(data.countryId())
                .orElseThrow(() -> new ResourceNotFoundException("Country not found."));
        if(Boolean.FALSE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is deleted.");
        }

        entitiesValidator.validate(genreDAO.findAllById(data.genreIds()), data.genreIds(), "Genre");
        entitiesValidator.validate(actorDAO.findAllById(data.mainActorIds()), data.mainActorIds(), "Actor");
        entitiesValidator.validate(directorDAO.findAllById(data.directorIds()), data.directorIds(), "Director");
        entitiesValidator.validate(languageDAO.findAllById(data.languageIds()), data.languageIds(), "Language");
        entitiesValidator.validate(streamingDAO.findAllById(data.streamingIds()), data.streamingIds(), "Streaming");

        Movie movie = new Movie(data, country);
        movie = movieDAO.save(movie);

        updateAllRelationships(movie.getId(), data.genreIds(), data.mainActorIds(), data.directorIds(), data.languageIds(), data.streamingIds());

        fetchAndSetAllRelationships(movie);

        return new GetMovieDetailsDTO(movie);
    }

    public Page<GetMovieDTO> getAllMovies(Pageable pageable){
        Page<Movie> moviePage = movieDAO.findAllAvailable(pageable);
        return moviePage.map(GetMovieDTO::new);
    }

    public GetMovieDetailsDTO getAMovie(Long id){
        Movie movie = movieDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Movie not found."));

        if(Boolean.FALSE.equals(movie.getIsAvailable())){
            throw new BusinessRuleException("Movie is deleted.");
        }

        fetchAndSetAllRelationships(movie);

        return new GetMovieDetailsDTO(movie);
    }

    @Transactional
    public GetMovieDetailsDTO putAMovie(Long id, PutMovieDTO data){
        Movie movie = movieDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Movie not found."));

        if(Boolean.FALSE.equals(movie.getIsAvailable())){
            throw new BusinessRuleException("Movie is deleted.");
        }

        Country country = null;
        List<Genre> genres = null;
        List<Actor> actors = null;
        List<Director> directors = null;
        List<Language> languages = null;
        List<Streaming> streamings = null;

        if(data.countryId() != null){
            country = countryDAO.findById(data.countryId())
                    .orElseThrow(() -> new ResourceNotFoundException("Country not found."));
            if(Boolean.FALSE.equals(country.getIsAvailable())){
                throw new BusinessRuleException("Country is deleted.");
            }
        }

        if(data.genreIds() != null) {
            genres = genreDAO.findAllById(data.genreIds());
            entitiesValidator.validate(genres, data.genreIds(), "Genre");
        }
        if(data.mainActorIds() != null) {
            actors = actorDAO.findAllById(data.mainActorIds());
            entitiesValidator.validate(actors, data.mainActorIds(), "Actor");
        }
        if(data.directorIds() != null) {
            directors = directorDAO.findAllById(data.directorIds());
            entitiesValidator.validate(directors, data.directorIds(), "Director");
        }
        if(data.languageIds() != null) {
            languages = languageDAO.findAllById(data.languageIds());
            entitiesValidator.validate(languages, data.languageIds(), "Language");
        }
        if(data.streamingIds() != null) {
            streamings = streamingDAO.findAllById(data.streamingIds());
            entitiesValidator.validate(streamings, data.streamingIds(), "Streaming");
        }

        movie.updateData(data, country, genres, actors, directors, languages, streamings);
        movieDAO.save(movie);

        updateAllRelationships(id, data.genreIds(), data.mainActorIds(), data.directorIds(), data.languageIds(), data.streamingIds());

        fetchAndSetAllRelationships(movie);

        return new GetMovieDetailsDTO(movie);
    }

    @Transactional
    public void deleteAMovie(Long id){
        Movie movie = movieDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Movie not found."));

        if(Boolean.FALSE.equals(movie.getIsAvailable())){
            throw new BusinessRuleException("Movie is already deleted.");
        }

        movie.delete();
        movieDAO.updateIsAvailable(id, movie.getIsAvailable());
    }

    @Transactional
    public GetMovieDetailsDTO reactivateAMovie(Long id){
        Movie movie = movieDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Movie not found."));

        if(Boolean.TRUE.equals(movie.getIsAvailable())){
            throw new BusinessRuleException("Movie is already active.");
        }

        movie.reactivate();
        movieDAO.updateIsAvailable(id, movie.getIsAvailable());

        fetchAndSetAllRelationships(movie);
        return new GetMovieDetailsDTO(movie);
    }
}
