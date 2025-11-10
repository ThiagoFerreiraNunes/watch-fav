import { useLocation } from "react-router-dom";
import * as S from "./styles";

type Props = {
  to: string;
  children: string;
};

export const NavLink = ({ to, children }: Props) => {
  const location = useLocation();
  const isThisPage: boolean = location.pathname == to;

  return (
    <S.Container>
      <S.StyledLink $isThisPage={isThisPage} to={to}>
        {children}
      </S.StyledLink>
    </S.Container>
  );
};
