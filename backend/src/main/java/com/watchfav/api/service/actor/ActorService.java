package com.watchfav.api.service.actor;

import com.watchfav.api.dto.actor.GetActorDTO;
import com.watchfav.api.dto.actor.PostActorDTO;
import com.watchfav.api.dto.actor.PutActorDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.model.Actor;
import com.watchfav.api.model.Country;
import com.watchfav.api.repository.ActorRepository;
import com.watchfav.api.repository.CountryRepository;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class ActorService {
    @Autowired
    private ActorRepository actorRepository;

    @Autowired
    private CountryRepository countryRepository;

    @Transactional
    public GetActorDTO postAnActor(PostActorDTO data){
        Country country = countryRepository.findById(data.countryId())
                .orElseThrow(() -> new EntityNotFoundException("Country not found."));

        if(Boolean.FALSE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is deleted.");
        }

        Actor actor = new Actor(data, country);
        actorRepository.save(actor);

        return new GetActorDTO(actor);
    }

    public List<GetActorDTO> getAllActors(){
        return actorRepository.findAllByAvailableAndSort().stream().map(GetActorDTO::new).toList();
    }

    public GetActorDTO getAnActor(Long id){
        Actor actor = actorRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Actor not found."));

        if(Boolean.FALSE.equals(actor.getIsAvailable())){
            throw new BusinessRuleException("Actor is deleted");
        }

        return new GetActorDTO(actor);
    }

    @Transactional
    public GetActorDTO putAnActor(Long id, PutActorDTO data){
        Actor actor = actorRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Actor not found."));

        if(Boolean.FALSE.equals(actor.getIsAvailable())){
            throw new BusinessRuleException("Actor is deleted");
        }

        Country country = null;

        if(data.countryId() != null){
            country = countryRepository.findById(data.countryId())
                    .orElseThrow(() -> new EntityNotFoundException("Country not found."));

            if(Boolean.FALSE.equals(country.getIsAvailable())){
                throw new BusinessRuleException("Country is deleted");
            }
        }

        actor.updateData(data, country);

        return new GetActorDTO(actor);
    }

    @Transactional
    public void deleteAnActor(Long id){
        Actor actor = actorRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Actor not found."));

        if(Boolean.FALSE.equals(actor.getIsAvailable())){
            throw new BusinessRuleException("Actor is already deleted");
        }

        actor.delete();
    }

    @Transactional
    public GetActorDTO reactivateAnActor(Long id){
        Actor actor = actorRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Actor not found."));

        if(Boolean.TRUE.equals(actor.getIsAvailable())){
            throw new BusinessRuleException("Actor is already activate");
        }

        actor.reactivate();
        return new GetActorDTO(actor);
    }
}
