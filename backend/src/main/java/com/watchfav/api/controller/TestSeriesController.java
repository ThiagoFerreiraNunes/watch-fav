package com.watchfav.api.controller;

import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/series")
public class TestSeriesController {

    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public Resource getSeriesJson() {
        return new ClassPathResource("series.json");
    }
}