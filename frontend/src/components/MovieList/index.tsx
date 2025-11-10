import * as S from "./styles";
import { MovieItem } from "./MovieItem";
import type { Movie } from "../../interfaces/Movie/Movie";
import { useEffect, useState } from "react";

export const MovieList = () => {
  const [movies, setMovies] = useState<Movie[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    fetch("http://localhost:8080/api/movies")
      .then((response) => {
        if (!response.ok) {
          throw new Error("Erro ao buscar filmes");
        }
        return response.json();
      })
      .then((data) => {
        setMovies(data.movies);
      })
      .catch((error) => {
        console.error("Erro:", error);
      })
      .finally(() => {
        setLoading(false);
      });
  }, []);

  if (loading) return <p>Carregando...</p>;

  return (
    <S.Container>
      {movies.map((movie) => (
        <MovieItem key={movie.id} movie={movie} />
      ))}
    </S.Container>
  );
};
