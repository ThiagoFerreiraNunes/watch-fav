import * as S from "./styles";
import icon from "../../assets/icon.png";
import { useNavigate } from "react-router-dom";
import { NavLink } from "./NavLink";

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
          <NavLink to="/home">Início</NavLink>
          <NavLink to="/movies">Filmes</NavLink>
          <NavLink to="/series">Séries</NavLink>
          <NavLink to="/suggestions">Sugestões</NavLink>
        </ul>
      </nav>
    </S.Container>
  );
};
