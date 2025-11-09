import { Filter } from "../../components/Filter";
import { Input } from "../../components/Input";
import { SeriesList } from "../../components/SeriesList";
import * as S from "./styles";

export const Series = () => {
  return (
    <S.Container>
      <div>
        <Input placeholder="Busque por series..." />
        <Filter />
      </div>
      <SeriesList />
    </S.Container>
  );
};
