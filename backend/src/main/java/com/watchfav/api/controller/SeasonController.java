package com.watchfav.api.controller;

import com.watchfav.api.dto.season.GetSeasonDTO;
import com.watchfav.api.dto.season.GetSeasonDetailsDTO;
import com.watchfav.api.dto.season.PostSeasonDTO;
import com.watchfav.api.dto.season.PutSeasonDTO;
import com.watchfav.api.service.season.SeasonService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.List;

@RestController
@RequestMapping("/series/{seriesId}/seasons")
public class SeasonController {

    @Autowired SeasonService seasonService;

    @PostMapping
    public ResponseEntity<GetSeasonDetailsDTO> postASeason(@PathVariable Long seriesId, @RequestBody @Valid PostSeasonDTO data, UriComponentsBuilder builder){
        GetSeasonDetailsDTO season = seasonService.postASeason(seriesId, data);
        URI uri = builder.path("/{seasonId}").buildAndExpand(season.id()).toUri();

        return ResponseEntity.created(uri).body(season);
    }

    @GetMapping
    public ResponseEntity<List<GetSeasonDTO>> getAllSeasons(@PathVariable Long seriesId){
        return ResponseEntity.ok(seasonService.getAllSeasons(seriesId));
    }

    @GetMapping("/{seasonId}")
    public ResponseEntity<GetSeasonDetailsDTO> getASeason(@PathVariable Long seriesId, @PathVariable Long seasonId){
        return ResponseEntity.ok(seasonService.getASeason(seriesId, seasonId));
    }

    @PutMapping("/{seasonId}")
    public ResponseEntity<GetSeasonDetailsDTO> putASeason(@PathVariable Long seriesId, @PathVariable Long seasonId, @RequestBody PutSeasonDTO data){
        return ResponseEntity.ok(seasonService.putASeason(seriesId, seasonId, data));
    }

    @DeleteMapping("/{seasonId}")
    public ResponseEntity<Void> deleteASeason(@PathVariable Long seriesId, @PathVariable Long seasonId){
        seasonService.deleteASeason(seriesId, seasonId);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/{seasonId}")
    public ResponseEntity<GetSeasonDetailsDTO> reactivateASeason(@PathVariable Long seriesId, @PathVariable Long seasonId){
        return ResponseEntity.ok(seasonService.reactivateASeason(seriesId, seasonId));
    }
}