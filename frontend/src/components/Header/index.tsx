import * as S from "./styles";
import icon from "../../assets/icon.png";
import { useNavigate } from "react-router-dom";

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
          <li>
            <S.StyledLink to="/home">Início</S.StyledLink>
          </li>
          <li>
            <S.StyledLink to="/movies">Filmes</S.StyledLink>
          </li>
          <li>
            <S.StyledLink to="/series">Séries</S.StyledLink>
          </li>
          <li>
            <S.StyledLink to="/suggestions">Sugestões</S.StyledLink>
          </li>
        </ul>
      </nav>
    </S.Container>
  );
};
