package com.watchfav.api.controller;

import com.watchfav.api.dto.director.GetDirectorDTO;
import com.watchfav.api.dto.director.PostDirectorDTO;
import com.watchfav.api.dto.director.PutDirectorDTO;
import com.watchfav.api.service.director.DirectorService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.List;

@RestController
@RequestMapping("/api/directors")
public class DirectorController {

    @Autowired
    private DirectorService directorService;

    @PostMapping
    public ResponseEntity<GetDirectorDTO> postADirector(@RequestBody @Valid PostDirectorDTO data, UriComponentsBuilder builder){
        GetDirectorDTO director = directorService.postADirector(data);
        URI uri = builder.path("/{id}").buildAndExpand(director.id()).toUri();

        return ResponseEntity.created(uri).body(director);
    }

    @GetMapping
    public ResponseEntity<List<GetDirectorDTO>> getAllDirectors(){
        return ResponseEntity.ok(directorService.getAllDirectors());
    }

    @GetMapping("/{id}")
    public ResponseEntity<GetDirectorDTO> getADirector(@PathVariable Long id){
        return ResponseEntity.ok(directorService.getADirector(id));
    }

    @PutMapping("/{id}")
    public ResponseEntity<GetDirectorDTO> putADirector(@PathVariable Long id, @RequestBody PutDirectorDTO data){
        return ResponseEntity.ok(directorService.putADirector(id, data));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteADirector(@PathVariable Long id){
        directorService.deleteADirector(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/{id}")
    public ResponseEntity<GetDirectorDTO> reactivateADirector(@PathVariable Long id){
        return ResponseEntity.ok(directorService.reactivateADirector(id));
    }
}