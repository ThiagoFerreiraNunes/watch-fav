import * as S from "./styles";
import { SeriesItem } from "./SeriesItem";
import type { Series } from "../../interfaces/Series/Series";
import { useEffect, useState } from "react";

export const SeriesList = () => {
  const [series, setSeries] = useState<Series[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    fetch("http://localhost:8080/api/series")
      .then((response) => {
        if (!response.ok) {
          throw new Error("Erro ao buscar series");
        }
        return response.json();
      })
      .then((data) => {
        setSeries(data.series);
      })
      .catch((error) => {
        console.error("Erro:", error);
      })
      .finally(() => {
        setLoading(false);
      });
  }, []);

  if (loading) return <p>Carregando...</p>;

  return (
    <S.Container>
      {series.map((series) => (
        <SeriesItem key={series.id} series={series} />
      ))}
    </S.Container>
  );
};
