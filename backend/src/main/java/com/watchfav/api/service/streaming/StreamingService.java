package com.watchfav.api.service.streaming;

import com.watchfav.api.dto.streaming.GetStreamingDTO;
import com.watchfav.api.dto.streaming.PostStreamingDTO;
import com.watchfav.api.dto.streaming.PutStreamingDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.exception.ResourceNotFoundException;
import com.watchfav.api.model.Streaming;
import com.watchfav.api.repository.StreamingDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class StreamingService {

    @Autowired
    private StreamingDAO streamingDAO;

    @Transactional
    public GetStreamingDTO postAStreaming(PostStreamingDTO data) {
        Streaming streaming = new Streaming(data);
        streaming = streamingDAO.save(streaming);
        return new GetStreamingDTO(streaming);
    }

    public List<GetStreamingDTO> getAllStreamings(){
        return streamingDAO.findAllAvailableAndSort().stream().map(GetStreamingDTO::new).toList();
    }

    public GetStreamingDTO getAStreaming(Long id){
        Streaming streaming = streamingDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Streaming not found"));

        if(Boolean.FALSE.equals(streaming.getIsAvailable())){
            throw new BusinessRuleException("Streaming is deleted.");
        }

        return new GetStreamingDTO(streaming);
    }

    @Transactional
    public GetStreamingDTO putAStreaming(Long id, PutStreamingDTO data){
        Streaming streaming = streamingDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Streaming not found"));

        if(Boolean.FALSE.equals(streaming.getIsAvailable())){
            throw new BusinessRuleException("Streaming is deleted.");
        }
        streaming.updateData(data);
        streamingDAO.save(streaming);
        return new GetStreamingDTO(streaming);
    }

    @Transactional
    public void deleteAStreaming(Long id){
        Streaming streaming = streamingDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Streaming not found"));

        if(Boolean.FALSE.equals(streaming.getIsAvailable())){
            throw new BusinessRuleException("Streaming is already deleted.");
        }
        streaming.delete();
        streamingDAO.updateIsAvailable(id, streaming.getIsAvailable());
    }

    @Transactional
    public GetStreamingDTO reactivateAStreaming(Long id){
        Streaming streaming = streamingDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Streaming not found"));

        if(Boolean.TRUE.equals(streaming.getIsAvailable())){
            throw new BusinessRuleException("Streaming is already active.");
        }
        streaming.reactivate();
        streamingDAO.updateIsAvailable(id, streaming.getIsAvailable());

        return new GetStreamingDTO(streaming);
    }
}
