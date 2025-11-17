package com.watchfav.api.service.language;

import com.watchfav.api.dto.language.GetLanguageDTO;
import com.watchfav.api.dto.language.PostLanguageDTO;
import com.watchfav.api.dto.language.PutLanguageDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.exception.ResourceNotFoundException;
import com.watchfav.api.model.Language;
import com.watchfav.api.repository.LanguageDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class LanguageService {

    @Autowired
    private LanguageDAO languageDAO;

    @Transactional
    public GetLanguageDTO postALanguage(PostLanguageDTO data){
        Language language = new Language(data);
        language = languageDAO.save(language);
        return new GetLanguageDTO(language);
    }

    public List<GetLanguageDTO> getAllLanguages(){
        return languageDAO.findAllAvailableAndSort().stream().map(GetLanguageDTO::new).toList();
    }

    public GetLanguageDTO getALanguage(Long id){
        Language language = languageDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Language not found."));

        if(Boolean.FALSE.equals(language.getIsAvailable())){
            throw new BusinessRuleException("Language is deleted.");
        }

        return new GetLanguageDTO(language);
    }

    @Transactional
    public GetLanguageDTO putALanguage(Long id, PutLanguageDTO data){
        Language language = languageDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Language not found."));

        if(Boolean.FALSE.equals(language.getIsAvailable())){
            throw new BusinessRuleException("Language is deleted.");
        }
        language.updateData(data);
        languageDAO.save(language);
        return new GetLanguageDTO(language);
    }

    @Transactional
    public void deleteALanguage(Long id){
        Language language = languageDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Language not found."));

        if(Boolean.FALSE.equals(language.getIsAvailable())){
            throw new BusinessRuleException("Language is already deleted.");
        }
        language.delete();
        languageDAO.updateIsAvailable(id, language.getIsAvailable());
    }

    @Transactional
    public GetLanguageDTO reactivateALanguage(Long id){
        Language language = languageDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Language not found."));

        if(Boolean.TRUE.equals(language.getIsAvailable())){
            throw new BusinessRuleException("Language is already active.");
        }
        language.reactivate();
        languageDAO.updateIsAvailable(id, language.getIsAvailable());
        return new GetLanguageDTO(language);
    }
}
