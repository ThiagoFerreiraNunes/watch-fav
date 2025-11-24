package com.watchfav.api.repository;

import com.watchfav.api.model.Director;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface DirectorRepository extends JpaRepository<Director, Long> {
    @Query("SELECT d FROM Director d WHERE d.isAvailable = true ORDER BY d.name")
    List<Director> findAllByAvailableAndSort();

    @Query("SELECT d FROM Director d WHERE LOWER(d.name) LIKE LOWER(CONCAT('%', :text, '%')) AND d.isAvailable = true")
    List<Director> findAllBySearch(@Param("text") String text);
}
