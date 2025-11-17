package com.watchfav.api.service.actor;

import com.watchfav.api.dto.actor.GetActorDTO;
import com.watchfav.api.dto.actor.PostActorDTO;
import com.watchfav.api.dto.actor.PutActorDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.exception.ResourceNotFoundException;
import com.watchfav.api.model.Actor;
import com.watchfav.api.model.Country;
import com.watchfav.api.repository.ActorDAO;
import com.watchfav.api.repository.CountryDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class ActorService {

    @Autowired
    private ActorDAO actorDAO;

    @Autowired
    private CountryDAO countryDAO;

    @Transactional
    public GetActorDTO postAnActor(PostActorDTO data){
        Country country = countryDAO.findById(data.countryId())
                .orElseThrow(() -> new ResourceNotFoundException("Country not found."));

        if(Boolean.FALSE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is deleted.");
        }

        Actor actor = new Actor(data, country);
        actor = actorDAO.save(actor);

        return new GetActorDTO(actor);
    }

    public List<GetActorDTO> getAllActors(){
        return actorDAO.findAllAvailableAndSort().stream().map(GetActorDTO::new).toList();
    }

    public GetActorDTO getAnActor(Long id){
        Actor actor = actorDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Actor not found."));

        if(Boolean.FALSE.equals(actor.getIsAvailable())){
            throw new BusinessRuleException("Actor is deleted");
        }

        return new GetActorDTO(actor);
    }

    @Transactional
    public GetActorDTO putAnActor(Long id, PutActorDTO data){
        Actor actor = actorDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Actor not found."));

        if(Boolean.FALSE.equals(actor.getIsAvailable())){
            throw new BusinessRuleException("Actor is deleted");
        }

        Country country = null;

        if(data.countryId() != null){
            country = countryDAO.findById(data.countryId())
                    .orElseThrow(() -> new ResourceNotFoundException("Country not found."));

            if(Boolean.FALSE.equals(country.getIsAvailable())){
                throw new BusinessRuleException("Country is deleted");
            }
        }
        actor.updateData(data, country);
        actorDAO.save(actor);

        return new GetActorDTO(actor);
    }

    @Transactional
    public void deleteAnActor(Long id){
        Actor actor = actorDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Actor not found."));

        if(Boolean.FALSE.equals(actor.getIsAvailable())){
            throw new BusinessRuleException("Actor is already deleted");
        }
        actor.delete();
        actorDAO.updateIsAvailable(id, actor.getIsAvailable());
    }

    @Transactional
    public GetActorDTO reactivateAnActor(Long id){
        Actor actor = actorDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Actor not found."));

        if(Boolean.TRUE.equals(actor.getIsAvailable())){
            throw new BusinessRuleException("Actor is already active");
        }

        actor.reactivate();
        actorDAO.updateIsAvailable(id, actor.getIsAvailable());
        return new GetActorDTO(actor);
    }
}
