import type { Movie } from "../../../interfaces/Movie/Movie";
import { formatDuration } from "../../../utils/formatDuration";
import * as S from "./styles";

type Props = {
  movie: Movie;
};

export const MovieItem = ({ movie }: Props) => {
  return (
    <S.Container>
      <div>
        <img src={movie.imageUrl} alt="imagem do filme" />
      </div>
      <h2>{movie.name}</h2>
      <p>Ano: {movie.releaseYear}</p>
      <p>Duração: {formatDuration(movie.duration)}</p>
      <p>Restrição: {movie.ageRating}</p>
    </S.Container>
  );
};
