package com.watchfav.api.service.director;

import com.watchfav.api.dto.director.GetDirectorDTO;
import com.watchfav.api.dto.director.PostDirectorDTO;
import com.watchfav.api.dto.director.PutDirectorDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.exception.ResourceNotFoundException;
import com.watchfav.api.model.Country;
import com.watchfav.api.model.Director;
import com.watchfav.api.repository.CountryDAO;
import com.watchfav.api.repository.CountryRepository;
import com.watchfav.api.repository.DirectorRepository;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class DirectorService {

    @Autowired
    private DirectorRepository directorRepository;

    @Autowired
    private CountryRepository countryRepository;

    @Transactional
    public GetDirectorDTO postADirector(PostDirectorDTO data){
        Country country = countryRepository.findById(data.countryId())
                .orElseThrow(() -> new EntityNotFoundException("Country not found."));

        if(Boolean.FALSE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is deleted.");
        }

        Director director = new Director(data, country);
        directorRepository.save(director);

        return new GetDirectorDTO(director);
    }

    public List<GetDirectorDTO> getAllDirectors(){
        return directorRepository.findAllByAvailableAndSort().stream().map(GetDirectorDTO::new).toList();
    }

    public GetDirectorDTO getADirector(Long id){
        Director director = directorRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Director not found."));

        if(Boolean.FALSE.equals(director.getIsAvailable())){
            throw new EntityNotFoundException("Director is deleted");
        }

        return new GetDirectorDTO(director);
    }

    @Transactional
    public GetDirectorDTO putADirector(Long id, PutDirectorDTO data){
        Director director = directorRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Director not found."));

        if(Boolean.FALSE.equals(director.getIsAvailable())){
            throw new EntityNotFoundException("Director is deleted");
        }

        Country country = null;

        if(data.countryId() != null){
            country = countryRepository.findById(data.countryId())
                    .orElseThrow(() -> new EntityNotFoundException("Country not found."));

            if(Boolean.FALSE.equals(country.getIsAvailable())){
                throw new EntityNotFoundException("Country is deleted");
            }
        }

        director.updateData(data, country);
        return new GetDirectorDTO(director);
    }

    @Transactional
    public void deleteADirector(Long id){
        Director director = directorRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Director not found."));

        if(Boolean.FALSE.equals(director.getIsAvailable())){
            throw new EntityNotFoundException("Director is already deleted.");
        }

        director.delete();
    }

    @Transactional
    public GetDirectorDTO reactivateADirector(Long id){
        Director director = directorRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Director not found."));

        if(Boolean.TRUE.equals(director.getIsAvailable())){
            throw new EntityNotFoundException("Director is already active.");
        }

        director.reactivate();
        return new GetDirectorDTO(director);
    }

}