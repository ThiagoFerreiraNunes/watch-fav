import type { Actor } from "../Actor/Actor";
import type { Director } from "../Director/Director";

export interface EpisodeDetails {
  id: number;
  number: number;
  name: string;
  duration: string;
  series: string;
  season: number;
  mainActors: Actor[];
  directors: Director[];
}
