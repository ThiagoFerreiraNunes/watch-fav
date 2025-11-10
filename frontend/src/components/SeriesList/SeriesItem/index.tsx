import type { Series } from "../../../interfaces/Series/Series";
import * as S from "./styles";

type Props = {
  series: Series;
};

export const SeriesItem = ({ series }: Props) => {
  return (
    <S.Container>
      <div>
        <img src={series.imageUrl} alt="imagem da serie" />
      </div>
      <h2>{series.name}</h2>
      <p>Ano: {series.releaseYear}</p>
      <p>Temporadas: {series.seasons}</p>
      <p>Restrição: {series.ageRating}</p>
    </S.Container>
  );
};
