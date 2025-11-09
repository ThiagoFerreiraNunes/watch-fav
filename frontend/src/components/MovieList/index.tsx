import * as S from "./styles";
import moviesData from "../../json/fakeApiMovies.json";
import type { Movie } from "../../interfaces/Movie/Movie";
import { MovieItem } from "./MovieItem";

interface MovieData {
  movies: Movie[];
}

const typedMoviesData = moviesData as MovieData;

export const MovieList = () => {
  return (
    <S.Container>
      {typedMoviesData.movies.map((movie) => (
        <MovieItem key={movie.id} movie={movie} />
      ))}
    </S.Container>
  );
};
