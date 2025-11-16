import * as S from "./styles";

type Props = {
  children: string;
};

export const AuthButton = ({ children }: Props) => {
  return <S.Container>{children}</S.Container>;
};
