import { useNavigate, useParams } from "react-router-dom";
import * as S from "./styles";
import { useEffect, useState } from "react";
import type { MovieDetails } from "../../interfaces/Movie/MovieDetails";
import { IoArrowBackSharp } from "react-icons/io5";
import { formatDuration } from "../../utils/formatDuration";
import { SimpleInformation } from "../../components/SimpleInformation";

export const SpecificMovie = () => {
  const param = useParams();
  const navigate = useNavigate();
  const [movie, setMovie] = useState<MovieDetails>();
  const [loading, setLoading] = useState(true);

  const handleBackClick = () => {
    navigate("/movies");
  };

  useEffect(() => {
    fetch(`http://localhost:8080/api/movies/${param.id}`)
      .then((response) => {
        if (!response.ok) {
          throw new Error("Erro ao buscar o filme");
        }
        return response.json();
      })
      .then((data) => {
        setMovie(data);
      })
      .catch((error) => {
        console.error("Erro:", error);
      })
      .finally(() => {
        setLoading(false);
      });
  }, [param.id]);

  if (loading) return <p>Carregando...</p>;

  return (
    <S.Container>
      <div>
        <IoArrowBackSharp className="back-icon" onClick={handleBackClick} />
        <aside>
          <h1>{movie?.name}</h1>
          <img src={movie?.imageUrl} alt={movie?.name} />
        </aside>
        <main>
          <h2>Informações</h2>
          <SimpleInformation desc="Descrição:" data={movie?.description} />
          <SimpleInformation desc="Ano:" data={movie?.releaseYear} />
          <SimpleInformation
            desc="Duração:"
            data={formatDuration(movie?.duration)}
          />
          <SimpleInformation desc="Restrição:" data={movie?.ageRating} />
          <SimpleInformation desc="País:" data={movie?.country.name} />
          <SimpleInformation
            desc="Gêneros:"
            data={movie?.genres.map(
              (genre, index, arr) =>
                genre.name + (index === arr.length - 1 ? "." : " - ")
            )}
          />
          <SimpleInformation
            desc="Idiomas Disponíveis:"
            data={movie?.languages.map(
              (language, index, arr) =>
                language.name + (index === arr.length - 1 ? "." : " - ")
            )}
          />
          <SimpleInformation
            desc="Streamings:"
            data={movie?.streamings.map(
              (streaming, index, arr) =>
                streaming.name + (index === arr.length - 1 ? "." : " - ")
            )}
          />
          <SimpleInformation
            desc="Diretores:"
            data={movie?.directors.map(
              (director, index, arr) =>
                director.name + (index === arr.length - 1 ? "." : " - ")
            )}
          />
          <SimpleInformation
            desc="Atores Principais:"
            data={movie?.mainActors.map(
              (actor, index, arr) =>
                actor.name + (index === arr.length - 1 ? "." : " - ")
            )}
          />
        </main>
      </div>
    </S.Container>
  );
};
