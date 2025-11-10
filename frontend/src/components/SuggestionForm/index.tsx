import { Button } from "../Button";
import * as S from "./styles";
import { SuggestionInput } from "./SuggestionInput";
import { SuggestionSelect } from "./SuggestionSelect";

export const SuggestionForm = () => {
  return (
    <S.Container>
      <SuggestionSelect />
      <SuggestionInput />
      <Button>Enviar</Button>
    </S.Container>
  );
};
