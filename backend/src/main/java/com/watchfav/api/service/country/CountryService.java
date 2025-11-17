package com.watchfav.api.service.country;

import com.watchfav.api.dto.country.PutCountryDTO;
import com.watchfav.api.exception.ResourceNotFoundException;
import com.watchfav.api.dto.country.GetCountryDTO;
import com.watchfav.api.dto.country.PostCountryDTO;
import com.watchfav.api.model.Country;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.repository.CountryDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class CountryService {

    @Autowired
    private CountryDAO countryDAO;

    @Transactional
    public GetCountryDTO postACountry(PostCountryDTO data){
        Country country = new Country(data);
        country = countryDAO.save(country);
        return new GetCountryDTO(country);
    }

    public List<GetCountryDTO> getAllCountries(){
        return countryDAO.findAllAvailableAndSort().stream().map(GetCountryDTO::new).toList();
    }

    public GetCountryDTO getACountry(Long id){
        Country country = countryDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Country not found."));

        if(Boolean.FALSE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is deleted.");
        }

        return new GetCountryDTO(country);
    }

    @Transactional
    public GetCountryDTO putACountry(Long id, PutCountryDTO data){
        Country country = countryDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Country not found."));

        if(Boolean.FALSE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is deleted.");
        }
        country.updateData(data);
        countryDAO.save(country);
        return new GetCountryDTO(country);
    }

    @Transactional
    public void deleteACountry(Long id){
        Country country = countryDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Country not found."));

        if(Boolean.FALSE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is already deleted.");
        }
        country.delete();
        countryDAO.updateIsAvailable(id, country.getIsAvailable());
    }

    @Transactional
    public GetCountryDTO reactivateACountry(Long id){
        Country country = countryDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Country not found."));

        if(Boolean.TRUE.equals(country.getIsAvailable())){
            throw new BusinessRuleException("Country is already active.");
        }
        country.reactivate();
        countryDAO.updateIsAvailable(id, country.getIsAvailable());
        return new GetCountryDTO(country);
    }
}
