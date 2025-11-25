package com.watchfav.api.service.series;

import com.watchfav.api.dto.series.GetSeriesDTO;
import com.watchfav.api.dto.series.GetSeriesDetailsDTO;
import com.watchfav.api.dto.series.PostSeriesDTO;
import com.watchfav.api.dto.series.PutSeriesDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.model.*;
import com.watchfav.api.repository.*;
import com.watchfav.api.service.commonValidation.EntitiesValidator;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class SeriesService {

    @Autowired CountryRepository countryRepository;
    @Autowired GenreRepository genreRepository;
    @Autowired LanguageRepository languageRepository;
    @Autowired StreamingRepository streamingRepository;
    @Autowired EntitiesValidator entitiesValidator;
    @Autowired SeriesRepository seriesRepository;

    @Transactional
    public GetSeriesDetailsDTO postASeries(PostSeriesDTO data){
        Country country = countryRepository.findById(data.countryId())
                .orElseThrow(() -> new EntityNotFoundException("Country not found."));

        if(Boolean.FALSE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is deleted.");
        }

        List<Genre> genres = genreRepository.findAllById(data.genreIds());
        entitiesValidator.validate(genres, data.genreIds(), "Genre");

        List<Language> languages = languageRepository.findAllById(data.languageIds());
        entitiesValidator.validate(languages, data.languageIds(), "Language");

        List<Streaming> streamings = streamingRepository.findAllById(data.streamingIds());
        entitiesValidator.validate(streamings, data.streamingIds(), "Streaming");

        Series series = new Series(data, country, genres, languages, streamings);
        seriesRepository.save(series);

        return new GetSeriesDetailsDTO(series);
    }

    public Page<GetSeriesDTO> getAllSeries(Pageable pageable){
        return seriesRepository.findAllByAvailable(pageable).map(GetSeriesDTO::new);
    }

    public Page<GetSeriesDTO> searchSeries(String text, Pageable pageable){
        return seriesRepository.findAllBySearch(text, pageable).map(GetSeriesDTO::new);
    }

    public GetSeriesDetailsDTO getASeries(Long id){
        Series series = seriesRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Series not found."));

        if(Boolean.FALSE.equals(series.getIsAvailable())){
            throw new BusinessRuleException("Series is deleted.");
        }

        return new GetSeriesDetailsDTO(series);
    }

    @Transactional
    public GetSeriesDetailsDTO putASeries(Long id, PutSeriesDTO data){
        Series series = seriesRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Series not found."));

        if(Boolean.FALSE.equals(series.getIsAvailable())){
            throw new BusinessRuleException("Series is deleted.");
        }

        Country country = null;
        List<Genre> genres = new ArrayList<>();
        List<Language> languages = new ArrayList<>();
        List<Streaming> streamings = new ArrayList<>();

        if(data.countryId() != null){
            country = countryRepository.findById(data.countryId())
                    .orElseThrow(() -> new EntityNotFoundException("Country not found."));

            if(Boolean.FALSE.equals(country.getIsAvailable())){
                throw new BusinessRuleException("Country is deleted.");
            }
        }

        if(data.genreIds() != null) {
            genres = genreRepository.findAllById(data.genreIds());
            entitiesValidator.validate(genres, data.genreIds(), "Genre");
        }

        if(data.languageIds() != null) {
            languages = languageRepository.findAllById(data.languageIds());
            entitiesValidator.validate(languages, data.languageIds(), "Language");
        }

        if(data.streamingIds() != null) {
            streamings = streamingRepository.findAllById(data.streamingIds());
            entitiesValidator.validate(streamings, data.streamingIds(), "Streaming");
        }

        series.updateData(data, country, genres, languages, streamings);
        return new GetSeriesDetailsDTO(series);
    }

    @Transactional
    public void deleteASeries(Long id){
        Series series = seriesRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Series not found."));

        if(Boolean.FALSE.equals(series.getIsAvailable())){
            throw new BusinessRuleException("Series is already deleted.");
        }

        series.delete();
    }

    @Transactional
    public GetSeriesDetailsDTO reactivateASeries(Long id){
        Series series = seriesRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Series not found."));

        if(Boolean.TRUE.equals(series.getIsAvailable())){
            throw new BusinessRuleException("Series is already active.");
        }

        series.reactivate();
        return new GetSeriesDetailsDTO(series);
    }
}
