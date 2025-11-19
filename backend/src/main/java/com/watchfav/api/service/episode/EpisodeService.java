package com.watchfav.api.service.episode;

import com.watchfav.api.dto.episode.GetEpisodeDTO;
import com.watchfav.api.dto.episode.GetEpisodeDetailsDTO;
import com.watchfav.api.dto.episode.PostEpisodeDTO;
import com.watchfav.api.dto.episode.PutEpisodeDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.model.Actor;
import com.watchfav.api.model.Director;
import com.watchfav.api.model.Episode;
import com.watchfav.api.model.Season;
import com.watchfav.api.repository.ActorRepository;
import com.watchfav.api.repository.DirectorRepository;
import com.watchfav.api.repository.EpisodeRepository;
import com.watchfav.api.repository.SeasonRepository;
import com.watchfav.api.service.commonValidation.EntitiesValidator;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class EpisodeService {
    @Autowired EpisodeRepository episodeRepository;
    @Autowired SeasonRepository seasonRepository;
    @Autowired ActorRepository actorRepository;
    @Autowired DirectorRepository directorRepository;
    @Autowired EntitiesValidator entitiesValidator;

    @Transactional
    public GetEpisodeDetailsDTO postAnEpisode(Long seasonId, PostEpisodeDTO data){
        Season season = seasonRepository.findById(seasonId)
                .orElseThrow(() -> new EntityNotFoundException("Season not found."));

        if(Boolean.FALSE.equals(season.getIsAvailable())){
            throw new BusinessRuleException("Season is deleted.");
        }

        List<Actor> actors = actorRepository.findAllById(data.mainActorIds());
        entitiesValidator.validate(actors, data.mainActorIds(), "Actor");

        List<Director> directors = directorRepository.findAllById(data.directorIds());
        entitiesValidator.validate(directors, data.directorIds(), "Director");

        Episode episode = new Episode(data, season, actors, directors);
        episodeRepository.save(episode);

        return new GetEpisodeDetailsDTO(episode);
    }

    public List<GetEpisodeDTO> getAllEpisodes(Long seasonId){
        Season season = seasonRepository.findById(seasonId)
                .orElseThrow(() -> new EntityNotFoundException("Season not found."));

        if(Boolean.FALSE.equals(season.getIsAvailable())){
            throw new BusinessRuleException("Season is deleted.");
        }
        return episodeRepository.findAllBySeasonIdAndAvailable(seasonId).stream().map(GetEpisodeDTO::new).toList();
    }

    public GetEpisodeDetailsDTO getAnEpisode(Long seasonId, Long episodeId){
        Season season = seasonRepository.findById(seasonId)
                .orElseThrow(() -> new EntityNotFoundException("Season not found."));

        if(Boolean.FALSE.equals(season.getIsAvailable())){
            throw new BusinessRuleException("Season is deleted.");
        }

        Episode episode = episodeRepository.findBySeasonIdAndEpisodeId(seasonId, episodeId);

        if(episode == null){
            throw new EntityNotFoundException("Episode not found.");
        }

        if(Boolean.FALSE.equals(episode.getIsAvailable())){
            throw new BusinessRuleException("Episode is deleted.");
        }

        return new GetEpisodeDetailsDTO(episode);
    }

    @Transactional
    public GetEpisodeDetailsDTO putAnEpisode(Long seasonId, Long episodeId, PutEpisodeDTO data){
        Season season = seasonRepository.findById(seasonId)
                .orElseThrow(() -> new EntityNotFoundException("Season not found."));

        if(Boolean.FALSE.equals(season.getIsAvailable())){
            throw new BusinessRuleException("Season is deleted.");
        }

        List<Actor> actors = actorRepository.findAllById(data.mainActorIds());
        entitiesValidator.validate(actors, data.mainActorIds(), "Actor");

        List<Director> directors = directorRepository.findAllById(data.directorIds());
        entitiesValidator.validate(directors, data.directorIds(), "Director");

        Episode episode = episodeRepository.findBySeasonIdAndEpisodeId(seasonId, episodeId);

        if(episode == null){
            throw new EntityNotFoundException("Episode not found.");
        }

        if(Boolean.FALSE.equals(episode.getIsAvailable())){
            throw new BusinessRuleException("Episode is deleted.");
        }

        episode.updateData(data, actors, directors);

        return new GetEpisodeDetailsDTO(episode);
    }

    @Transactional
    public void deleteAnEpisode(Long seasonId, Long episodeId){
        Season season = seasonRepository.findById(seasonId)
                .orElseThrow(() -> new EntityNotFoundException("Season not found."));

        if(Boolean.FALSE.equals(season.getIsAvailable())){
            throw new BusinessRuleException("Season is deleted.");
        }

        Episode episode = episodeRepository.findBySeasonIdAndEpisodeId(seasonId, episodeId);

        if(episode == null){
            throw new EntityNotFoundException("Episode not found.");
        }

        if(Boolean.FALSE.equals(episode.getIsAvailable())){
            throw new BusinessRuleException("Episode is already deleted.");
        }

        episode.delete();
    }

    @Transactional
    public GetEpisodeDetailsDTO reactivateAnEpisode(Long seasonId, Long episodeId){
        Season season = seasonRepository.findById(seasonId)
                .orElseThrow(() -> new EntityNotFoundException("Season not found."));

        if(Boolean.FALSE.equals(season.getIsAvailable())){
            throw new BusinessRuleException("Season is deleted.");
        }

        Episode episode = episodeRepository.findBySeasonIdAndEpisodeId(seasonId, episodeId);

        if(episode == null){
            throw new EntityNotFoundException("Episode not found.");
        }

        if(Boolean.TRUE.equals(episode.getIsAvailable())){
            throw new BusinessRuleException("Episode is already active.");
        }

        season.reactivate();

        return new GetEpisodeDetailsDTO(episode);
    }
}
