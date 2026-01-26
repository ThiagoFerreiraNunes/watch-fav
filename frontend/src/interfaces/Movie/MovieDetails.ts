import type { Actor } from "../Actor/Actor";
import type { Country } from "../Country/Country";
import type { Director } from "../Director/Director";
import type { Genre } from "../Genre/Genre";
import type { Language } from "../Language/Language";
import type { Streaming } from "../Streaming/Streaming";

export interface MovieDetails {
  id: number;
  name: string;
  imageUrl: string;
  description: string;
  releaseYear: number;
  duration: string;
  ageRating: string;
  country: Country;
  genres: Genre[];
  mainActors: Actor[];
  directors: Director[];
  languages: Language[];
  streamings: Streaming[];
}
