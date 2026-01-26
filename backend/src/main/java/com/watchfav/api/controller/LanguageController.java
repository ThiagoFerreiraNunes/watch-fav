package com.watchfav.api.controller;

import com.watchfav.api.dto.language.GetLanguageDTO;
import com.watchfav.api.dto.language.PostLanguageDTO;
import com.watchfav.api.dto.language.PutLanguageDTO;
import com.watchfav.api.service.language.LanguageService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.List;

@RestController
@RequestMapping("/api/languages")
public class LanguageController {

    @Autowired
    private LanguageService languageService;

    @PostMapping
    public ResponseEntity<GetLanguageDTO> postALanguage(@RequestBody @Valid PostLanguageDTO data, UriComponentsBuilder builder){
        GetLanguageDTO language = languageService.postALanguage(data);
        URI uri = builder.path("/{id}").buildAndExpand(language.id()).toUri();

        return ResponseEntity.created(uri).body(language);
    }

    @GetMapping
    public ResponseEntity<List<GetLanguageDTO>> getAllLanguages(){
        return ResponseEntity.ok(languageService.getAllLanguages());
    }

    @GetMapping("/search")
    public ResponseEntity<List<GetLanguageDTO>> searchLanguages(@RequestParam String text){
        return ResponseEntity.ok(languageService.searchLanguages(text));
    }

    @GetMapping("/{id}")
    public ResponseEntity<GetLanguageDTO> getALanguage(@PathVariable Long id){
        return ResponseEntity.ok(languageService.getALanguage(id));
    }

    @PutMapping("/{id}")
    public ResponseEntity<GetLanguageDTO> putALanguage(@PathVariable Long id, @RequestBody PutLanguageDTO data){
        return ResponseEntity.ok(languageService.putALanguage(id, data));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteALanguage(@PathVariable Long id){
        languageService.deleteALanguage(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/{id}")
    public ResponseEntity<GetLanguageDTO> reactivateALanguage(@PathVariable Long id){
        return ResponseEntity.ok(languageService.reactivateALanguage(id));
    }
}
