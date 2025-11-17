package com.watchfav.api.service.genre;

import com.watchfav.api.dto.genre.GetGenreDTO;
import com.watchfav.api.dto.genre.PostGenreDTO;
import com.watchfav.api.dto.genre.PutGenreDTO;
import com.watchfav.api.exception.BusinessRuleException;
import com.watchfav.api.exception.ResourceNotFoundException;
import com.watchfav.api.model.Genre;
import com.watchfav.api.repository.GenreDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class GenreService {

    @Autowired
    private GenreDAO genreDAO;

    @Transactional
    public GetGenreDTO postAGenre(PostGenreDTO data){
        Genre genre = new Genre(data);
        genre = genreDAO.save(genre);
        return new GetGenreDTO(genre);
    }

    public List<GetGenreDTO> getAllGenres(){
        return genreDAO.findAllAvailableAndSort().stream().map(GetGenreDTO::new).toList();
    }

    public GetGenreDTO getAGenre(Long id){
        Genre genre = genreDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Genre not found."));

        if(Boolean.FALSE.equals(genre.getIsAvailable())){
            throw new BusinessRuleException("Genre is deleted.");
        }

        return new GetGenreDTO(genre);
    }

    @Transactional
    public GetGenreDTO putAGenre(Long id, PutGenreDTO data){
        Genre genre = genreDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Genre not found."));

        if(Boolean.FALSE.equals(genre.getIsAvailable())){
            throw new BusinessRuleException("Genre is deleted.");
        }
        genre.updateData(data);
        genreDAO.save(genre);
        return new GetGenreDTO(genre);
    }

    @Transactional
    public void deleteAGenre(Long id){
        Genre genre = genreDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Genre not found."));

        if(Boolean.FALSE.equals(genre.getIsAvailable())){
            throw new BusinessRuleException("Genre is already deleted.");
        }
        genre.delete();
        genreDAO.updateIsAvailable(id, genre.getIsAvailable());
    }

    @Transactional
    public GetGenreDTO reactivateAGenre(Long id){
        Genre genre = genreDAO.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Genre not found."));

        if(Boolean.TRUE.equals(genre.getIsAvailable())){
            throw new BusinessRuleException("Genre is already active.");
        }
        genre.reactivate();
        genreDAO.updateIsAvailable(id, genre.getIsAvailable());
        return new GetGenreDTO(genre);
    }
}