import { Filter } from "../../components/Filter";
import { Input } from "../../components/Input";
import { MovieList } from "../../components/MovieList";
import * as S from "./styles";

export const Movies = () => {
  return (
    <S.Container>
      <div>
        <Input placeholder="Busque por filmes..." />
        <Filter />
      </div>
      <MovieList />
    </S.Container>
  );
};
