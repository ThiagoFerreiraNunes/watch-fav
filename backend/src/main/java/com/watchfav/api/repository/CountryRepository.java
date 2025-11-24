package com.watchfav.api.repository;

import com.watchfav.api.model.Country;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface CountryRepository extends JpaRepository<Country, Long> {

    @Query("SELECT c FROM Country c WHERE c.isAvailable = true ORDER BY c.name")
    List<Country> findAllByAvailableAndSort();

    @Query("SELECT c FROM Country c WHERE LOWER(c.name) LIKE LOWER(CONCAT('%', :text, '%')) AND c.isAvailable = true")
    List<Country> findAllBySearch(@Param("text") String text);
}
