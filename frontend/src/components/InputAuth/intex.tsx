import * as S from "./styles";
import type { ReactNode } from "react";

type Props = {
  type: string;
  placeholder: string;
  children: ReactNode;
};

export const InputAuth = ({ type, placeholder, children }: Props) => {
  return (
    <S.Container>
      {children}
      <input type={type} placeholder={placeholder} />
    </S.Container>
  );
};
