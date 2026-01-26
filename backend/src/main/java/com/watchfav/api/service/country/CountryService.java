package com.watchfav.api.service.country;

import com.watchfav.api.dto.country.PutCountryDTO;
import com.watchfav.api.dto.country.GetCountryDTO;
import com.watchfav.api.dto.country.PostCountryDTO;
import com.watchfav.api.model.Country;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.repository.CountryRepository;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class CountryService {

    @Autowired
    private CountryRepository countryRepository;

    @Transactional
    public GetCountryDTO postACountry(PostCountryDTO data){
        Country country = new Country(data);
        countryRepository.save(country);

        return new GetCountryDTO(country);
    }

    public List<GetCountryDTO> getAllCountries(){
        return countryRepository.findAllByAvailableAndSort().stream().map(GetCountryDTO::new).toList();
    }

    public List<GetCountryDTO> searchCountries(String text) {
        return countryRepository.findAllBySearch(text).stream().map(GetCountryDTO::new).toList();
    }

    public GetCountryDTO getACountry(Long id){
        Country country = countryRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Country not found."));

        if(Boolean.FALSE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is deleted.");
        }

        return new GetCountryDTO(country);
    }

    @Transactional
    public GetCountryDTO putACountry(Long id, PutCountryDTO data){
        Country country = countryRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Country not found."));

        if(Boolean.FALSE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is deleted.");
        }

        country.updateData(data);
        return new GetCountryDTO(country);
    }

    @Transactional
    public void deleteACountry(Long id){
        Country country = countryRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Country not found."));

        if(Boolean.FALSE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is already deleted.");
        }

        country.delete();
    }

    @Transactional
    public GetCountryDTO reactivateACountry(Long id){
        Country country = countryRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Country not found."));

        if(Boolean.TRUE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is already active.");
        }

        country.reactivate();
        return new GetCountryDTO(country);
    }
}
