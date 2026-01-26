package com.watchfav.api.model;

public enum AgeRating {
    FREE("Livre"), TEN("10"), TWELVE("12"), FOURTEEN("14"), SIXTEEN("16"), EIGHTEEN("18");

    private final String name;

    AgeRating(String name){
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
