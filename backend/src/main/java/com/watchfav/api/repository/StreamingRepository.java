package com.watchfav.api.repository;

import com.watchfav.api.model.Streaming;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface StreamingRepository extends JpaRepository<Streaming, Long> {
    @Query("SELECT s FROM Streaming s WHERE s.isAvailable = true ORDER BY s.name")
    List<Streaming> findAllByAvailableAndSort();

    @Query("SELECT s FROM Streaming s WHERE LOWER(s.name) LIKE LOWER(CONCAT('%', :text, '%')) AND s.isAvailable = true")
    List<Streaming> findAllBySearch(@Param("text") String text);
}
