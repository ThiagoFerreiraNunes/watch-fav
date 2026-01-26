package com.watchfav.api.controller;

import com.watchfav.api.dto.series.GetSeriesDTO;
import com.watchfav.api.dto.series.GetSeriesDetailsDTO;
import com.watchfav.api.dto.series.PostSeriesDTO;
import com.watchfav.api.dto.series.PutSeriesDTO;
import com.watchfav.api.service.series.SeriesService;
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
@RequestMapping("/api/series")
public class SeriesController {

    @Autowired private SeriesService seriesService;

    @PostMapping
    public ResponseEntity<GetSeriesDetailsDTO> postASeries(@RequestBody @Valid PostSeriesDTO data, UriComponentsBuilder builder){
        GetSeriesDetailsDTO series = seriesService.postASeries(data);
        URI uri = builder.path("/{id}").buildAndExpand(series.id()).toUri();

        return ResponseEntity.created(uri).body(series);
    }

    @GetMapping
    public ResponseEntity<Page<GetSeriesDTO>> getAllSeries(@PageableDefault(size = 20, sort = {"name"}) Pageable pageable){
        return ResponseEntity.ok(seriesService.getAllSeries(pageable));
    }

    @GetMapping("/search")
    public ResponseEntity<Page<GetSeriesDTO>> searchSeries(@RequestParam String text, @PageableDefault(size = 20, sort = {"name"}) Pageable pageable){
        return ResponseEntity.ok(seriesService.searchSeries(text, pageable));
    }

    @GetMapping("/{id}")
    public ResponseEntity<GetSeriesDetailsDTO> getASeries(@PathVariable Long id){
        return ResponseEntity.ok(seriesService.getASeries(id));
    }

    @PutMapping("/{id}")
    public ResponseEntity<GetSeriesDetailsDTO> putASeries(@PathVariable Long id, @RequestBody PutSeriesDTO data){
        return ResponseEntity.ok(seriesService.putASeries(id, data));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteASeries(@PathVariable Long id){
        seriesService.deleteASeries(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/{id}")
    public ResponseEntity<GetSeriesDetailsDTO> reactivateASeries(@PathVariable Long id){
        return ResponseEntity.ok(seriesService.reactivateASeries(id));
    }
}
