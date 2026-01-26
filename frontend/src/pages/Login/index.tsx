import { LoginForm } from "../../components/LoginForm";
import { StyledLink } from "../../components/StyledLink/styles";
import * as S from "./styles";

export const Login = () => {
  return (
    <S.Container>
      <main>
        <LoginForm />
      </main>

      <aside>
        <div>
          <h2>Não tem uma conta?</h2>
          <p>Cadastre-se no botão abaixo!</p>
          <StyledLink to="/register">Cadastre-se</StyledLink>
        </div>
      </aside>
    </S.Container>
  );
};
