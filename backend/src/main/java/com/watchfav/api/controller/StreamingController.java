package com.watchfav.api.controller;

import com.watchfav.api.dto.streaming.GetStreamingDTO;
import com.watchfav.api.dto.streaming.PostStreamingDTO;
import com.watchfav.api.dto.streaming.PutStreamingDTO;
import com.watchfav.api.service.streaming.StreamingService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.List;

@RestController
@RequestMapping("/api/streamings")
public class StreamingController {

    @Autowired
    private StreamingService streamingService;

    @PostMapping
    public ResponseEntity<GetStreamingDTO> postAStreaming(@RequestBody @Valid PostStreamingDTO data, UriComponentsBuilder builder){
        GetStreamingDTO streaming = streamingService.postAStreaming(data);
        URI uri = builder.path("/{id}").buildAndExpand(streaming.id()).toUri();

        return ResponseEntity.created(uri).body(streaming);
    }

    @GetMapping
    public ResponseEntity<List<GetStreamingDTO>> getAllStreamings(){
        return ResponseEntity.ok(streamingService.getAllStreamings());
    }

    @GetMapping("/{id}")
    public ResponseEntity<GetStreamingDTO> getAStreaming(@PathVariable Long id){
        return ResponseEntity.ok(streamingService.getAStreaming(id));
    }

    @PutMapping("/{id}")
    public ResponseEntity<GetStreamingDTO> putAStreaming(@PathVariable Long id, @RequestBody PutStreamingDTO data){
        return ResponseEntity.ok(streamingService.putAStreaming(id, data));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteAStreaming(@PathVariable Long id){
        streamingService.deleteAStreaming(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/{id}")
    public ResponseEntity<GetStreamingDTO> reactivateAStreaming(@PathVariable Long id){
        return ResponseEntity.ok(streamingService.reactivateAStreaming(id));
    }
}
