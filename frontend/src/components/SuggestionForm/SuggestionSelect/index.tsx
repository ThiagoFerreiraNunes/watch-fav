import * as S from "./styles";

export const SuggestionSelect = () => {
  return (
    <S.Container>
      <p>Tipo</p>
      <select>
        <option value=""></option>
        <option value="movie">Filme</option>
        <option value="series">Serie</option>
      </select>
    </S.Container>
  );
};
