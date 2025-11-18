package com.watchfav.api.service.language;

import com.watchfav.api.dto.language.GetLanguageDTO;
import com.watchfav.api.dto.language.PostLanguageDTO;
import com.watchfav.api.dto.language.PutLanguageDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.model.Language;
import com.watchfav.api.repository.LanguageRepository;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class LanguageService {

    @Autowired private LanguageRepository languageRepository;

    @Transactional
    public GetLanguageDTO postALanguage(PostLanguageDTO data){
        Language language = new Language(data);
        languageRepository.save(language);

        return new GetLanguageDTO(language);
    }

    public List<GetLanguageDTO> getAllLanguages(){
        return languageRepository.findAllByAvailableAndSort().stream().map(GetLanguageDTO::new).toList();
    }

    public GetLanguageDTO getALanguage(Long id){
        Language language = languageRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Language not found."));

        if(Boolean.FALSE.equals(language.getIsAvailable())){
            throw new BusinessRuleException("Language is deleted.");
        }

        return new GetLanguageDTO(language);
    }

    @Transactional
    public GetLanguageDTO putALanguage(Long id, PutLanguageDTO data){
        Language language = languageRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Language not found."));

        if(Boolean.FALSE.equals(language.getIsAvailable())){
            throw new BusinessRuleException("Language is deleted.");
        }

        language.updateData(data);

        return new GetLanguageDTO(language);
    }

    @Transactional
    public void deleteALanguage(Long id){
        Language language = languageRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Language not found."));

        if(Boolean.FALSE.equals(language.getIsAvailable())){
            throw new BusinessRuleException("Language is already deleted.");
        }

        language.delete();
    }

    @Transactional
    public GetLanguageDTO reactivateALanguage(Long id){
        Language language = languageRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Language not found."));

        if(Boolean.TRUE.equals(language.getIsAvailable())){
            throw new BusinessRuleException("Language is already active.");
        }

        language.reactivate();

        return new GetLanguageDTO(language);
    }
}
