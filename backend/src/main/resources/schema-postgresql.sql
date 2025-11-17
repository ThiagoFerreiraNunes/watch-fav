CREATE TABLE IF NOT EXISTS tb_users (
    user_id BIGSERIAL PRIMARY KEY,
    user_name VARCHAR(50) NOT NULL,
    user_email VARCHAR(100) NOT NULL UNIQUE,
    user_password VARCHAR(255) NOT NULL,
    is_available BOOLEAN NOT NULL
);

CREATE TABLE IF NOT EXISTS tb_roles (
    role_id BIGSERIAL PRIMARY KEY,
    role_name VARCHAR(50) NOT NULL UNIQUE,
    is_available BOOLEAN NOT NULL
);

CREATE TABLE IF NOT EXISTS tb_user_roles (
    user_id BIGINT NOT NULL,
    role_id BIGINT NOT NULL,
    PRIMARY KEY (user_id, role_id),
    FOREIGN KEY (user_id) REFERENCES tb_users(user_id),
    FOREIGN KEY (role_id) REFERENCES tb_roles(role_id)
);

CREATE TABLE IF NOT EXISTS tb_countries (
    country_id BIGSERIAL PRIMARY KEY,
    country_name VARCHAR(100) NOT NULL UNIQUE,
    is_available BOOLEAN NOT NULL
);

CREATE TABLE IF NOT EXISTS tb_series (
    series_id BIGSERIAL PRIMARY KEY,
    series_name VARCHAR(100) NOT NULL,
    series_description TEXT NOT NULL,
    image_url VARCHAR(2048) NOT NULL,
    release_year INT NOT NULL,
    age_rating VARCHAR(50) NOT NULL,
    country_id BIGINT NOT NULL,
    is_available BOOLEAN NOT NULL,
    FOREIGN KEY (country_id) REFERENCES tb_countries(country_id)
);

CREATE TABLE IF NOT EXISTS tb_seasons (
    season_id BIGSERIAL PRIMARY KEY,
    season_number INT NOT NULL,
    release_year INT NOT NULL,
    series_id BIGINT NOT NULL,
    is_available BOOLEAN NOT NULL,
    FOREIGN KEY (series_id) REFERENCES tb_series(series_id)
);

CREATE TABLE IF NOT EXISTS tb_episodes (
    episode_id BIGSERIAL PRIMARY KEY,
    episode_number INT NOT NULL,
    episode_name VARCHAR(100) NOT NULL,
    duration TIME NOT NULL,
    season_id BIGINT NOT NULL,
    is_available BOOLEAN NOT NULL,
    FOREIGN KEY (season_id) REFERENCES tb_seasons(season_id)
);

CREATE TABLE IF NOT EXISTS tb_movies (
    movie_id BIGSERIAL PRIMARY KEY,
    movie_name VARCHAR(100) NOT NULL,
    movie_description TEXT NOT NULL,
    image_url VARCHAR(2048) NOT NULL,
    release_year INT NOT NULL,
    duration TIME NOT NULL,
    age_rating VARCHAR(50) NOT NULL,
    country_id BIGINT NOT NULL,
    is_available BOOLEAN NOT NULL,
    FOREIGN KEY (country_id) REFERENCES tb_countries(country_id)
);

CREATE TABLE IF NOT EXISTS tb_streamings (
    streaming_id BIGSERIAL PRIMARY KEY,
    streaming_name VARCHAR(100) NOT NULL UNIQUE,
    streaming_url VARCHAR(2048) NOT NULL,
    is_available BOOLEAN NOT NULL
);

CREATE TABLE IF NOT EXISTS tb_genres (
    genre_id BIGSERIAL PRIMARY KEY,
    genre_name VARCHAR(50) NOT NULL UNIQUE,
    is_available BOOLEAN NOT NULL
);

CREATE TABLE IF NOT EXISTS tb_languages (
    language_id BIGSERIAL PRIMARY KEY,
    language_name VARCHAR(50) NOT NULL UNIQUE,
    is_available BOOLEAN NOT NULL
);

CREATE TABLE IF NOT EXISTS tb_actors (
    actor_id BIGSERIAL PRIMARY KEY,
    actor_name VARCHAR(100) NOT NULL,
    country_id BIGINT NOT NULL,
    is_available BOOLEAN NOT NULL,
    FOREIGN KEY (country_id) REFERENCES tb_countries(country_id)
);

CREATE TABLE IF NOT EXISTS tb_directors (
    director_id BIGSERIAL PRIMARY KEY,
    director_name VARCHAR(100) NOT NULL,
    country_id BIGINT NOT NULL,
    is_available BOOLEAN NOT NULL,
    FOREIGN KEY (country_id) REFERENCES tb_countries(country_id)
);

CREATE TABLE IF NOT EXISTS tb_user_favorite_movies (
    user_id BIGINT NOT NULL,
    movie_id BIGINT NOT NULL,
    PRIMARY KEY (user_id, movie_id),
    FOREIGN KEY (user_id) REFERENCES tb_users(user_id),
    FOREIGN KEY (movie_id) REFERENCES tb_movies(movie_id)
);

CREATE TABLE IF NOT EXISTS tb_user_favorite_series (
    user_id BIGINT NOT NULL,
    series_id BIGINT NOT NULL,
    PRIMARY KEY (user_id, series_id),
    FOREIGN KEY (user_id) REFERENCES tb_users(user_id),
    FOREIGN KEY (series_id) REFERENCES tb_series(series_id)
);

CREATE TABLE IF NOT EXISTS tb_movies_streamings (
    movie_id BIGINT NOT NULL,
    streaming_id BIGINT NOT NULL,
    PRIMARY KEY (movie_id, streaming_id),
    FOREIGN KEY (movie_id) REFERENCES tb_movies(movie_id),
    FOREIGN KEY (streaming_id) REFERENCES tb_streamings(streaming_id)
);

CREATE TABLE IF NOT EXISTS tb_series_streamings (
    series_id BIGINT NOT NULL,
    streaming_id BIGINT NOT NULL,
    PRIMARY KEY (series_id, streaming_id),
    FOREIGN KEY (series_id) REFERENCES tb_series(series_id),
    FOREIGN KEY (streaming_id) REFERENCES tb_streamings(streaming_id)
);

CREATE TABLE IF NOT EXISTS tb_movies_genres (
    movie_id BIGINT NOT NULL,
    genre_id BIGINT NOT NULL,
    PRIMARY KEY (movie_id, genre_id),
    FOREIGN KEY (movie_id) REFERENCES tb_movies(movie_id),
    FOREIGN KEY (genre_id) REFERENCES tb_genres(genre_id)
);

CREATE TABLE IF NOT EXISTS tb_series_genres (
    series_id BIGINT NOT NULL,
    genre_id BIGINT NOT NULL,
    PRIMARY KEY (series_id, genre_id),
    FOREIGN KEY (series_id) REFERENCES tb_series(series_id),
    FOREIGN KEY (genre_id) REFERENCES tb_genres(genre_id)
);

CREATE TABLE IF NOT EXISTS tb_movies_languages (
    movie_id BIGINT NOT NULL,
    language_id BIGINT NOT NULL,
    PRIMARY KEY (movie_id, language_id),
    FOREIGN KEY (movie_id) REFERENCES tb_movies(movie_id),
    FOREIGN KEY (language_id) REFERENCES tb_languages(language_id)
);

CREATE TABLE IF NOT EXISTS tb_series_languages (
    series_id BIGINT NOT NULL,
    language_id BIGINT NOT NULL,
    PRIMARY KEY (series_id, language_id),
    FOREIGN KEY (series_id) REFERENCES tb_series(series_id),
    FOREIGN KEY (language_id) REFERENCES tb_languages(language_id)
);

CREATE TABLE IF NOT EXISTS tb_movies_actors (
    movie_id BIGINT NOT NULL,
    actor_id BIGINT NOT NULL,
    PRIMARY KEY (movie_id, actor_id),
    FOREIGN KEY (movie_id) REFERENCES tb_movies(movie_id),
    FOREIGN KEY (actor_id) REFERENCES tb_actors(actor_id)
);

CREATE TABLE IF NOT EXISTS tb_episodes_actors (
    episode_id BIGINT NOT NULL,
    actor_id BIGINT NOT NULL,
    PRIMARY KEY (episode_id, actor_id),
    FOREIGN KEY (episode_id) REFERENCES tb_episodes(episode_id),
    FOREIGN KEY (actor_id) REFERENCES tb_actors(actor_id)
);

CREATE TABLE IF NOT EXISTS tb_movies_directors (
    movie_id BIGINT NOT NULL,
    director_id BIGINT NOT NULL,
    PRIMARY KEY (movie_id, director_id),
    FOREIGN KEY (movie_id) REFERENCES tb_movies(movie_id),
    FOREIGN KEY (director_id) REFERENCES tb_directors(director_id)
);

CREATE TABLE IF NOT EXISTS tb_episodes_directors (
    episode_id BIGINT NOT NULL,
    director_id BIGINT NOT NULL,
    PRIMARY KEY (episode_id, director_id),
    FOREIGN KEY (episode_id) REFERENCES tb_episodes(episode_id),
    FOREIGN KEY (director_id) REFERENCES tb_directors(director_id)
);