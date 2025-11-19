package com.watchfav.api.controller;

import com.watchfav.api.dto.episode.GetEpisodeDTO;
import com.watchfav.api.dto.episode.GetEpisodeDetailsDTO;
import com.watchfav.api.dto.episode.PostEpisodeDTO;
import com.watchfav.api.dto.episode.PutEpisodeDTO;
import com.watchfav.api.service.episode.EpisodeService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.List;

@RestController
@RequestMapping("/seasons/{seasonId}/episodes")
public class EpisodeController {

    @Autowired
    private EpisodeService episodeService;

    @PostMapping
    public ResponseEntity<GetEpisodeDetailsDTO> postAnEpisode(@PathVariable Long seasonId, @RequestBody @Valid PostEpisodeDTO data, UriComponentsBuilder builder){
        GetEpisodeDetailsDTO episode = episodeService.postAnEpisode(seasonId, data);
        URI uri = builder.path("/{episodeId}").buildAndExpand(episode.id()).toUri();

        return ResponseEntity.created(uri).body(episode);
    }

    @GetMapping
    public ResponseEntity<List<GetEpisodeDTO>> getAllEpisodes(@PathVariable Long seasonId){
        return ResponseEntity.ok(episodeService.getAllEpisodes(seasonId));
    }

    @GetMapping("/{episodeId}")
    public ResponseEntity<GetEpisodeDetailsDTO> getAnEpisode(@PathVariable Long seasonId, @PathVariable Long episodeId){
        return ResponseEntity.ok(episodeService.getAnEpisode(seasonId, episodeId));
    }

    @PutMapping("/{episodeId}")
    public ResponseEntity<GetEpisodeDetailsDTO> putAnEpisode(@PathVariable Long seasonId, @PathVariable Long episodeId, @RequestBody PutEpisodeDTO data){
        return ResponseEntity.ok(episodeService.putAnEpisode(seasonId, episodeId, data));
    }

    @DeleteMapping("/{episodeId}")
    public ResponseEntity<Void> deleteAnEpisode(@PathVariable Long seasonId, @PathVariable Long episodeId){
        episodeService.deleteAnEpisode(seasonId, episodeId);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/{episodeId}")
    public ResponseEntity<GetEpisodeDetailsDTO> reactivateAnEpisode(@PathVariable Long seasonId, @PathVariable Long episodeId){
        return ResponseEntity.ok(episodeService.reactivateAnEpisode(seasonId, episodeId));
    }
}
