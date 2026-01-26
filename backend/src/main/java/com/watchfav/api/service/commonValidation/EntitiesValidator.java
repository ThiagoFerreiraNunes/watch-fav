package com.watchfav.api.service.commonValidation;

import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.model.common.HasAvailability;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class EntitiesValidator {

    public <T extends HasAvailability> void validate(List<T> entities, List<Long> ids, String entityName){

        if(entities.size() != ids.size()){
            throw new EntityNotFoundException(entityName + " not found.");
        }

        boolean hasInactive = entities.stream().anyMatch(e -> Boolean.FALSE.equals(e.getIsAvailable()));

        if(hasInactive){
            throw new BusinessRuleException(entityName + " is deleted.");
        }
    }
}
