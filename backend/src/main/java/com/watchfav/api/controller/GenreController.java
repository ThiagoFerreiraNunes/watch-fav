package com.watchfav.api.controller;

import com.watchfav.api.dto.genre.GetGenreDTO;
import com.watchfav.api.dto.genre.PostGenreDTO;
import com.watchfav.api.dto.genre.PutGenreDTO;
import com.watchfav.api.service.genre.GenreService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.List;

@RestController
@RequestMapping("/api/genres")
public class GenreController {

    @Autowired
    private GenreService genreService;

    @PostMapping
    public ResponseEntity<GetGenreDTO> postAGenre(@RequestBody @Valid PostGenreDTO data, UriComponentsBuilder builder){
        GetGenreDTO genre = genreService.postAGenre(data);
        URI uri = builder.path("/{id}").buildAndExpand(genre.id()).toUri();

        return ResponseEntity.created(uri).body(genre);
    }

    @GetMapping
    public ResponseEntity<List<GetGenreDTO>> getAllGenres(){
        return ResponseEntity.ok(genreService.getAllGenres());
    }

    @GetMapping("/{id}")
    public ResponseEntity<GetGenreDTO> getAGenre(@PathVariable Long id){
        return ResponseEntity.ok(genreService.getAGenre(id));
    }

    @PutMapping("/{id}")
    public ResponseEntity<GetGenreDTO> putAGenre(@PathVariable Long id, @RequestBody PutGenreDTO data){
        return ResponseEntity.ok(genreService.putAGenre(id, data));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteAGenre(@PathVariable Long id){
        genreService.deleteAGenre(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/{id}")
    public ResponseEntity<GetGenreDTO> reactivateAGenre(@PathVariable Long id){
        return ResponseEntity.ok(genreService.reactivateAGenre(id));
    }
}