import * as S from "./styles";

type Props = {
  children: string;
};

export const Button = ({ children }: Props) => {
  return <S.Container>{children}</S.Container>;
};
