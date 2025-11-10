import { SuggestionForm } from "../../components/SuggestionForm";
import * as S from "./styles";

export const Suggestions = () => {
  return (
    <S.Container>
      <h1>Dê aqui as suas sugestões</h1>
      <SuggestionForm />
    </S.Container>
  );
};
