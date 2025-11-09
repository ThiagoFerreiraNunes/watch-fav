import * as S from "./styles";
import seriesData from "../../json/fakeApiSeries.json";
import type { Series } from "../../interfaces/Series/Series";
import { SeriesItem } from "./SeriesItem";

interface SeriesData {
  series: Series[];
}

const typedSeriesData = seriesData as SeriesData;

export const SeriesList = () => {
  return (
    <S.Container>
      {typedSeriesData.series.map((series) => (
        <SeriesItem key={series.id} series={series} />
      ))}
    </S.Container>
  );
};
