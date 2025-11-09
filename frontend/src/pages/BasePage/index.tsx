import { Outlet } from "react-router-dom";
import { Header } from "../../components/Header";
import * as S from "./styles";
import { Footer } from "../../components/Footer";

export const BasePage = () => {
  return (
    <S.Container>
      <Header />
      <Outlet />
      <Footer />
    </S.Container>
  );
};
