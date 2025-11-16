import { RegisterFrom } from "../../components/RegisterForm";
import { StyledLink } from "../../components/StyledLink/styles";
import * as S from "./styles";

export const Register = () => {
  return (
    <S.Container>
      <main>
        <RegisterFrom />
      </main>

      <aside>
        <div>
          <h2>Já tem uma conta?</h2>
          <p>Faça seu o login no botão abaixo!</p>
          <StyledLink to="/login">Login</StyledLink>
        </div>
      </aside>
    </S.Container>
  );
};
