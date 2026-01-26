package com.watchfav.api.service.season;

import com.watchfav.api.dto.season.GetSeasonDTO;
import com.watchfav.api.dto.season.GetSeasonDetailsDTO;
import com.watchfav.api.dto.season.PostSeasonDTO;
import com.watchfav.api.dto.season.PutSeasonDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.model.Season;
import com.watchfav.api.model.Series;
import com.watchfav.api.repository.SeasonRepository;
import com.watchfav.api.repository.SeriesRepository;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class SeasonService {

    @Autowired SeriesRepository seriesRepository;
    @Autowired SeasonRepository seasonRepository;

    @Transactional
    public GetSeasonDetailsDTO postASeason(Long seriesId, PostSeasonDTO data){
        Series series = seriesRepository.findById(seriesId)
                .orElseThrow(() -> new EntityNotFoundException("Series not found."));

        if(Boolean.FALSE.equals(series.getIsAvailable())){
            throw new BusinessRuleException("Series is deleted.");
        }

        Season season = new Season(series, data);
        seasonRepository.save(season);

        return new GetSeasonDetailsDTO(season);
    }

    public List<GetSeasonDTO> getAllSeasons(Long seriesId){
        Series series = seriesRepository.findById(seriesId)
                .orElseThrow(() -> new EntityNotFoundException("Series not found."));

        if(Boolean.FALSE.equals(series.getIsAvailable())){
            throw new BusinessRuleException("Series is deleted.");
        }
        return seasonRepository.findAllBySeriesIdAndAvailable(seriesId).stream().map(GetSeasonDTO::new).toList();
    }

    public GetSeasonDetailsDTO getASeason(Long seriesId, Long seasonId){
        Series series = seriesRepository.findById(seriesId)
                .orElseThrow(() -> new EntityNotFoundException("Series not found."));

        if(Boolean.FALSE.equals(series.getIsAvailable())){
            throw new BusinessRuleException("Series is deleted.");
        }

        Season season = seasonRepository.findBySeriesIdAndSeasonId(seriesId, seasonId);

        if(season == null){
            throw new EntityNotFoundException("Season not found.");
        }

        if(Boolean.FALSE.equals(season.getIsAvailable())){
            throw new BusinessRuleException("Season is deleted.");
        }

        return new GetSeasonDetailsDTO(season);
    }

    @Transactional
    public GetSeasonDetailsDTO putASeason(Long seriesId, Long seasonId, PutSeasonDTO data){
        Series series = seriesRepository.findById(seriesId)
                .orElseThrow(() -> new EntityNotFoundException("Series not found."));

        if(Boolean.FALSE.equals(series.getIsAvailable())){
            throw new BusinessRuleException("Series is deleted.");
        }

        Season season = seasonRepository.findBySeriesIdAndSeasonId(seriesId, seasonId);

        if(season == null){
            throw new EntityNotFoundException("Season not found.");
        }

        if(Boolean.FALSE.equals(season.getIsAvailable())){
            throw new BusinessRuleException("Season is deleted.");
        }

        season.updateData(data);

        return new GetSeasonDetailsDTO(season);
    }

    @Transactional
    public void deleteASeason(Long seriesId, Long seasonId){
        Series series = seriesRepository.findById(seriesId)
                .orElseThrow(() -> new EntityNotFoundException("Series not found."));

        if(Boolean.FALSE.equals(series.getIsAvailable())){
            throw new BusinessRuleException("Series is deleted.");
        }

        Season season = seasonRepository.findBySeriesIdAndSeasonId(seriesId, seasonId);

        if(season == null){
            throw new EntityNotFoundException("Season not found.");
        }

        if(Boolean.FALSE.equals(season.getIsAvailable())){
            throw new BusinessRuleException("Season is already deleted.");
        }

        season.delete();
    }

    @Transactional
    public GetSeasonDetailsDTO reactivateASeason(Long seriesId, Long seasonId){
        Series series = seriesRepository.findById(seriesId)
                .orElseThrow(() -> new EntityNotFoundException("Series not found."));

        if(Boolean.FALSE.equals(series.getIsAvailable())){
            throw new BusinessRuleException("Series is deleted.");
        }

        Season season = seasonRepository.findBySeriesIdAndSeasonId(seriesId, seasonId);

        if(season == null){
            throw new EntityNotFoundException("Season not found.");
        }

        if(Boolean.TRUE.equals(season.getIsAvailable())){
            throw new BusinessRuleException("Season is already active.");
        }

        season.reactivate();

        return new GetSeasonDetailsDTO(season);
    }
}
