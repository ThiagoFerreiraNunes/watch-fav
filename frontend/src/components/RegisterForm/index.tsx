import { MdOutlineEmail } from "react-icons/md";
import { InputAuth } from "../InputAuth/intex";
import * as S from "./styles";
import { TbLockPassword } from "react-icons/tb";
import { AuthButton } from "../AuthButton";
import { FaRegUser } from "react-icons/fa";

export const RegisterFrom = () => {
  return (
    <S.Container>
      <h1>Faça seu Cadastro</h1>

      <InputAuth type="text" placeholder="Nome">
        <FaRegUser size={24} color="grey" />
      </InputAuth>
      <InputAuth type="email" placeholder="Email">
        <MdOutlineEmail size={24} color="grey" />
      </InputAuth>
      <InputAuth type="password" placeholder="Senha">
        <TbLockPassword size={24} color="grey" />
      </InputAuth>
      <InputAuth type="password" placeholder="Confirme sua senha">
        <TbLockPassword size={24} color="grey" />
      </InputAuth>
      <AuthButton>Cadastrar</AuthButton>
    </S.Container>
  );
};
