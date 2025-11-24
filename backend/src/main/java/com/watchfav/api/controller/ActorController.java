package com.watchfav.api.controller;

import com.watchfav.api.dto.actor.GetActorDTO;
import com.watchfav.api.dto.actor.PostActorDTO;
import com.watchfav.api.dto.actor.PutActorDTO;
import com.watchfav.api.service.actor.ActorService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.List;

@RestController
@RequestMapping("/api/actors")
public class ActorController {

    @Autowired
    private ActorService actorService;

    @PostMapping
    public ResponseEntity<GetActorDTO> postAnActor(@RequestBody @Valid PostActorDTO data, UriComponentsBuilder builder){
        GetActorDTO actor = actorService.postAnActor(data);
        URI uri = builder.path("/{id}").buildAndExpand(actor.id()).toUri();

        return ResponseEntity.created(uri).body(actor);
    }

    @GetMapping
    public ResponseEntity<List<GetActorDTO>> getAllActors(){
        return ResponseEntity.ok(actorService.getAllActors());
    }

    @GetMapping("/search")
    public ResponseEntity<List<GetActorDTO>> searchActors(@RequestParam String text){
        return ResponseEntity.ok(actorService.searchActors(text));
    }

    @GetMapping("/{id}")
    public ResponseEntity<GetActorDTO> getAnActor(@PathVariable Long id){
        return ResponseEntity.ok(actorService.getAnActor(id));
    }

    @PutMapping("/{id}")
    public ResponseEntity<GetActorDTO> putAnActor(@PathVariable Long id, @RequestBody PutActorDTO data){
        return ResponseEntity.ok(actorService.putAnActor(id, data));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteAnActor(@PathVariable Long id){
        actorService.deleteAnActor(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/{id}")
    public ResponseEntity<GetActorDTO> reactivateAnActor(@PathVariable Long id){
        return ResponseEntity.ok(actorService.reactivateAnActor(id));
    }
}
