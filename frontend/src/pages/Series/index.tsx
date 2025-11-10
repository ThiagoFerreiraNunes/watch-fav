import { Filter } from "../../components/Filter";
import { SearchInput } from "../../components/SearchInput";
import { SeriesList } from "../../components/SeriesList";
import * as S from "./styles";

export const Series = () => {
  return (
    <S.Container>
      <div>
        <SearchInput placeholder="Busque por series..." />
        <Filter />
      </div>
      <SeriesList />
    </S.Container>
  );
};
