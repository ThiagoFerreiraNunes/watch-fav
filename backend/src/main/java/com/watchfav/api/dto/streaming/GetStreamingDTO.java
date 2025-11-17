package com.watchfav.api.dto.streaming;

import com.watchfav.api.model.Streaming;

public record GetStreamingDTO(
        Long id,
        String name,
        String url
) {
    public GetStreamingDTO(Streaming streaming){
        this(
                streaming.getId(),
                streaming.getName(),
                streaming.getUrl()
        );
    }
}

