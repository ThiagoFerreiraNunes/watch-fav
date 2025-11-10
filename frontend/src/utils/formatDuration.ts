export const formatDuration = (duration: string): string => {
  const parts = duration.split(":");

  if (parts.length < 2) return duration;

  const hours = parseInt(parts[0], 10);
  const minutes = parseInt(parts[1], 10);

  let result = "";

  if (hours > 0) {
    result += `${hours}h `;
  }
  if (minutes > 0) {
    result += `${minutes}min`;
  }

  return result.trim() || "Duração indefinida";
};
