package com.watchfav.api.controller;

import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@CrossOrigin(origins = "http://localhost:5173")
@RestController
@RequestMapping("/api/movies")
public class TestMovieController {

    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public Resource getMoviesJson() {
        return new ClassPathResource("movies.json");
    }
}
