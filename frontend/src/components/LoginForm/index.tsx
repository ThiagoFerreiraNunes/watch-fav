import { AuthButton } from "../AuthButton";
import { InputAuth } from "../InputAuth/intex";
import { MdOutlineEmail } from "react-icons/md";
import { TbLockPassword } from "react-icons/tb";

import * as S from "./styles";

export const LoginForm = () => {
  return (
    <S.Container>
      <h1>Faça seu login</h1>
      <InputAuth type="email" placeholder="Email">
        <MdOutlineEmail size={24} color="grey" />
      </InputAuth>
      <InputAuth type="password" placeholder="Senha">
        <TbLockPassword size={24} color="grey" />
      </InputAuth>
      <AuthButton>Entrar</AuthButton>
    </S.Container>
  );
};
