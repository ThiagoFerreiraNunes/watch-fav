package com.watchfav.api.service.genre;

import com.watchfav.api.dto.genre.GetGenreDTO;
import com.watchfav.api.dto.genre.PostGenreDTO;
import com.watchfav.api.dto.genre.PutGenreDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.model.Genre;
import com.watchfav.api.repository.GenreRepository;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class GenreService {

    @Autowired private GenreRepository genreRepository;

    @Transactional
    public GetGenreDTO postAGenre(PostGenreDTO data){
        Genre genre = new Genre(data);
        genreRepository.save(genre);

        return new GetGenreDTO(genre);
    }

    public List<GetGenreDTO> getAllGenres(){
        return genreRepository.findAllByAvailableAndSort().stream().map(GetGenreDTO::new).toList();
    }

    public GetGenreDTO getAGenre(Long id){
        Genre genre = genreRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Genre not found."));

        if(Boolean.FALSE.equals(genre.getIsAvailable())){
            throw new BusinessRuleException("Genre is deleted.");
        }

        return new GetGenreDTO(genre);
    }

    @Transactional
    public GetGenreDTO putAGenre(Long id, PutGenreDTO data){
        Genre genre = genreRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Genre not found."));

        if(Boolean.FALSE.equals(genre.getIsAvailable())){
            throw new BusinessRuleException("Genre is deleted.");
        }

        genre.updateData(data);
        return new GetGenreDTO(genre);
    }

    @Transactional
    public void deleteAGenre(Long id){
        Genre genre = genreRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Genre not found."));;

        if(Boolean.FALSE.equals(genre.getIsAvailable())){
            throw new BusinessRuleException("Genre is already deleted.");
        }

        genre.delete();
    }

    @Transactional
    public GetGenreDTO reactivateAGenre(Long id){
        Genre genre = genreRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Genre not found."));

        if(Boolean.TRUE.equals(genre.getIsAvailable())){
            throw new BusinessRuleException("Genre is already active.");
        }

        genre.reactivate();
        return new GetGenreDTO(genre);
    }

}