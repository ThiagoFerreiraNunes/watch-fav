import * as S from "./styles";
import homeImage from "../../assets/home-img.png";

export const Home = () => {
  return (
    <S.Container>
      <main>
        <h1>Bem-vindo(a) ao WatchFav!</h1>
        <div>
          <p>
            Cansado de perder a conta dos filmes e séries incríveis que você
            planeja assistir? Aquele momento de procurar algo novo e não
            conseguir lembrar da lista que você montou mentalmente acabou!
          </p>
          <p>
            O WatchFav é a sua nova aplicação web, desenhada para ser o seu
            gerenciador definitivo de entretenimento. A missão é simples:
            oferecer a você uma maneira prática, organizada e intuitiva de
            acompanhar tudo o que você ama no mundo do cinema e das séries.
          </p>
        </div>
      </main>
      <aside>
        <img src={homeImage} alt="logo WatchFav" />
      </aside>
    </S.Container>
  );
};
