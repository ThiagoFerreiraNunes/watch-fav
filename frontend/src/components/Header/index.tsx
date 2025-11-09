import * as S from "./styles";
import icon from "../../assets/icon.png";
import { useNavigate } from "react-router-dom";
import { StyledLink } from "./StyledLink";

export const Header = () => {
  const navigate = useNavigate();

  const handleClick = () => {
    navigate("/home");
  };

  return (
    <S.Container>
      <div onClick={handleClick}>
        <img src={icon} alt="logo WatchFav" />
        <h2>WatchFav</h2>
      </div>

      <nav>
        <ul>
          <StyledLink to="/home">Início</StyledLink>
          <StyledLink to="/movies">Filmes</StyledLink>
          <StyledLink to="/series">Séries</StyledLink>
          <StyledLink to="/suggestions">Sugestões</StyledLink>
        </ul>
      </nav>
    </S.Container>
  );
};
