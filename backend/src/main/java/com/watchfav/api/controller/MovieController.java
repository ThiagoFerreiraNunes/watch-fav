package com.watchfav.api.controller;

import com.watchfav.api.dto.movies.GetMovieDTO;
import com.watchfav.api.dto.movies.GetMovieDetailsDTO;
import com.watchfav.api.dto.movies.PostMovieDTO;
import com.watchfav.api.dto.movies.PutMovieDTO;
import com.watchfav.api.service.movie.MovieService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;

@RestController
@RequestMapping("/api/movies")
public class MovieController {

    @Autowired
    private MovieService movieService;

    @PostMapping
    public ResponseEntity<GetMovieDetailsDTO> postAMovie(@RequestBody @Valid PostMovieDTO data, UriComponentsBuilder builder){
        GetMovieDetailsDTO movie = movieService.postAMovie(data);
        URI uri = builder.path("/{id}").buildAndExpand(movie.id()).toUri();

        return ResponseEntity.created(uri).body(movie);
    }

    @GetMapping
    public ResponseEntity<Page<GetMovieDTO>> getAllMovies(@PageableDefault(size = 20, sort = {"name"}) Pageable pageable){
        return ResponseEntity.ok(movieService.getAllMovies(pageable));
    }

    @GetMapping("/{id}")
    public ResponseEntity<GetMovieDetailsDTO> getAMovie(@PathVariable Long id){
        return ResponseEntity.ok(movieService.getAMovie(id));
    }

    @PutMapping("/{id}")
    public ResponseEntity<GetMovieDetailsDTO> putAMovie(@PathVariable Long id, @RequestBody PutMovieDTO data){
        return ResponseEntity.ok(movieService.putAMovie(id, data));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteAMovie(@PathVariable Long id){
        movieService.deleteAMovie(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/{id}")
    public ResponseEntity<GetMovieDetailsDTO> reactivateAMovie(@PathVariable Long id){
        return ResponseEntity.ok(movieService.reactivateAMovie(id));
    }
}
