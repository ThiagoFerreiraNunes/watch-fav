package com.watchfav.api.controller;

import com.watchfav.api.dto.country.GetCountryDTO;
import com.watchfav.api.dto.country.PostCountryDTO;
import com.watchfav.api.dto.country.PutCountryDTO;
import com.watchfav.api.service.country.CountryService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.List;

@RestController
@RequestMapping("/api/countries")
public class CountryController {

    @Autowired
    private CountryService countryService;

    @PostMapping
    public ResponseEntity<GetCountryDTO> postACountry(@RequestBody @Valid PostCountryDTO data, UriComponentsBuilder builder){
        GetCountryDTO country = countryService.postACountry(data);
        URI uri = builder.path("/{id}").buildAndExpand(country.id()).toUri();

        return ResponseEntity.created(uri).body(country);
    }

    @GetMapping
    public ResponseEntity<List<GetCountryDTO>> getAllCountries(){
        return ResponseEntity.ok(countryService.getAllCountries());
    }

    @GetMapping("/search")
    public ResponseEntity<List<GetCountryDTO>> searchCountries(@RequestParam String text){
        return ResponseEntity.ok(countryService.searchCountries(text));
    }

    @GetMapping("/{id}")
    public ResponseEntity<GetCountryDTO> getACountry(@PathVariable Long id){
        return ResponseEntity.ok(countryService.getACountry(id));
    }

    @PutMapping("/{id}")
    public ResponseEntity<GetCountryDTO> putACountry(@PathVariable Long id, @RequestBody PutCountryDTO data){
        return ResponseEntity.ok(countryService.putACountry(id, data));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteACountry(@PathVariable Long id){
        countryService.deleteACountry(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/{id}")
    public ResponseEntity<GetCountryDTO> reactivateACountry(@PathVariable Long id){
        return ResponseEntity.ok(countryService.reactivateACountry(id));
    }
}