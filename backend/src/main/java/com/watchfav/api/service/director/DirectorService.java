package com.watchfav.api.service.director;

import com.watchfav.api.dto.director.GetDirectorDTO;
import com.watchfav.api.dto.director.PostDirectorDTO;
import com.watchfav.api.dto.director.PutDirectorDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.exception.ResourceNotFoundException;
import com.watchfav.api.model.Country;
import com.watchfav.api.model.Director;
import com.watchfav.api.repository.CountryDAO;
import com.watchfav.api.repository.DirectorDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class DirectorService {

    @Autowired
    private DirectorDAO directorDAO;

    @Autowired
    private CountryDAO countryDAO;

    @Transactional
    public GetDirectorDTO postADirector(PostDirectorDTO data){
        Country country = countryDAO.findById(data.countryId())
                .orElseThrow(() -> new ResourceNotFoundException("Country not found."));

        if(Boolean.FALSE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is deleted.");
        }

        Director director = new Director(data, country);
        director = directorDAO.save(director);

        return new GetDirectorDTO(director);
    }

    public List<GetDirectorDTO> getAllDirectors(){
        return directorDAO.findAllAvailableAndSort().stream().map(GetDirectorDTO::new).toList();
    }

    public GetDirectorDTO getADirector(Long id){
        Director director = directorDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Director not found."));

        if(Boolean.FALSE.equals(director.getIsAvailable())){
            throw new BusinessRuleException("Director is deleted");
        }

        return new GetDirectorDTO(director);
    }

    @Transactional
    public GetDirectorDTO putADirector(Long id, PutDirectorDTO data){
        Director director = directorDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Director not found."));

        if(Boolean.FALSE.equals(director.getIsAvailable())){
            throw new BusinessRuleException("Director is deleted");
        }

        Country country = null;

        if(data.countryId() != null){
            country = countryDAO.findById(data.countryId())
                    .orElseThrow(() -> new ResourceNotFoundException("Country not found."));

            if(Boolean.FALSE.equals(country.getIsAvailable())){
                throw new BusinessRuleException("Country is deleted");
            }
        }

        director.updateData(data, country);
        directorDAO.save(director);
        return new GetDirectorDTO(director);
    }

    @Transactional
    public void deleteADirector(Long id){
        Director director = directorDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Director not found."));

        if(Boolean.FALSE.equals(director.getIsAvailable())){
            throw new BusinessRuleException("Director is already deleted.");
        }
        director.delete();
        directorDAO.updateIsAvailable(id, director.getIsAvailable());
    }

    @Transactional
    public GetDirectorDTO reactivateADirector(Long id){
        Director director = directorDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Director not found."));

        if(Boolean.TRUE.equals(director.getIsAvailable())){
            throw new BusinessRuleException("Director is already active.");
        }

        director.reactivate();
        directorDAO.updateIsAvailable(id, director.getIsAvailable());
        return new GetDirectorDTO(director);
    }
}