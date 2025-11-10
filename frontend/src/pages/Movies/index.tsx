import { Filter } from "../../components/Filter";
import { SearchInput } from "../../components/SearchInput";
import { MovieList } from "../../components/MovieList";
import * as S from "./styles";

export const Movies = () => {
  return (
    <S.Container>
      <div>
        <SearchInput placeholder="Busque por filmes..." />
        <Filter />
      </div>
      <MovieList />
    </S.Container>
  );
};
