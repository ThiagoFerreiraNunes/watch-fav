package com.watchfav.api.repository;

import com.watchfav.api.model.Country;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface CountryRepository extends JpaRepository<Country, Long> {

    @Query("SELECT c FROM Country c WHERE c.isAvailable = true ORDER BY c.name")
    List<Country> findAllByAvailableAndSort();
}
