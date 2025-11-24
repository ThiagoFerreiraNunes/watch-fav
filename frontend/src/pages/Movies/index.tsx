import { SearchInput } from "../../components/SearchInput";
import { MovieList } from "../../components/MovieList";
import * as S from "./styles";

export const Movies = () => {
  return (
    <S.Container>
      <SearchInput placeholder="Busque por filmes..." />
      <MovieList />
    </S.Container>
  );
};
