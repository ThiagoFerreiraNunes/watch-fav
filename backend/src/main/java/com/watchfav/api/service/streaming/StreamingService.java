package com.watchfav.api.service.streaming;

import com.watchfav.api.dto.streaming.GetStreamingDTO;
import com.watchfav.api.dto.streaming.PostStreamingDTO;
import com.watchfav.api.dto.streaming.PutStreamingDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.model.Streaming;
import com.watchfav.api.repository.StreamingRepository;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class StreamingService {

    @Autowired
    StreamingRepository streamingRepository;

    @jakarta.transaction.Transactional
    public GetStreamingDTO postAStreaming(PostStreamingDTO data) {
        Streaming streaming = new Streaming(data);
        streamingRepository.save(streaming);

        return new GetStreamingDTO(streaming);
    }

    public List<GetStreamingDTO> getAllStreamings(){
        return streamingRepository.findAllByAvailableAndSort().stream().map(GetStreamingDTO::new).toList();
    }

    public List<GetStreamingDTO> searchStreamings(String text) {
        return streamingRepository.findAllBySearch(text).stream().map(GetStreamingDTO::new).toList();
    }

    public GetStreamingDTO getAStreaming(Long id){
        Streaming streaming = streamingRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Streaming not found"));

        if(Boolean.FALSE.equals(streaming.getIsAvailable())){
            throw new BusinessRuleException("Streaming is deleted.");
        }

        return new GetStreamingDTO(streaming);
    }

    @jakarta.transaction.Transactional
    public GetStreamingDTO putAStreaming(Long id, PutStreamingDTO data){
        Streaming streaming = streamingRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Streaming not found"));

        if(Boolean.FALSE.equals(streaming.getIsAvailable())){
            throw new BusinessRuleException("Streaming is deleted.");
        }

        streaming.updateData(data);
        return new GetStreamingDTO(streaming);
    }

    @jakarta.transaction.Transactional
    public void deleteAStreaming(Long id){
        Streaming streaming = streamingRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Streaming not found"));

        if(Boolean.FALSE.equals(streaming.getIsAvailable())){
            throw new BusinessRuleException("Streaming is already deleted.");
        }

        streaming.delete();
    }

    @Transactional
    public GetStreamingDTO reactivateAStreaming(Long id){
        Streaming streaming = streamingRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Streaming not found"));

        if(Boolean.TRUE.equals(streaming.getIsAvailable())){
            throw new BusinessRuleException("Streaming is already active.");
        }

        streaming.reactivate();
        return new GetStreamingDTO(streaming);
    }
}
