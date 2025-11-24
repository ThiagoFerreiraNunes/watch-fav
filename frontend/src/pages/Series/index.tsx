import { SearchInput } from "../../components/SearchInput";
import { SeriesList } from "../../components/SeriesList";
import * as S from "./styles";

export const Series = () => {
  return (
    <S.Container>
      <SearchInput placeholder="Busque por séries..." />
      <SeriesList />
    </S.Container>
  );
};
