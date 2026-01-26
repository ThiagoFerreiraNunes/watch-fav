import type { Country } from "../Country/Country";
import type { Genre } from "../Genre/Genre";
import type { Language } from "../Language/Language";
import type { Season } from "../Season/Season";
import type { Streaming } from "../Streaming/Streaming";

export interface SeriesDetails {
  id: number;
  name: string;
  imageUrl: string;
  description: string;
  releaseYear: number;
  ageRating: string;
  country: Country;
  genres: Genre[];
  languages: Language[];
  seasons: Season[];
  streamings: Streaming[];
}
