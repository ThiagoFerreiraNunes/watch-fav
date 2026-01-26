package com.watchfav.api.repository;

import com.watchfav.api.model.Language;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface LanguageRepository extends JpaRepository<Language, Long> {
    @Query("SELECT l FROM Language l WHERE l.isAvailable = true ORDER BY l.name")
    List<Language> findAllByAvailableAndSort();

    @Query("SELECT l FROM Language l WHERE LOWER(l.name) LIKE LOWER(CONCAT('%', :text, '%')) AND l.isAvailable = true")
    List<Language> findAllBySearch(@Param("text") String text);
}
