import * as S from "./styles";

type Props = {
  desc: string;
  data: string | number | string[] | undefined;
};

export const SimpleInformation = ({ desc, data }: Props) => {
  return (
    <S.Container>
      <strong>{desc}</strong> {data}
    </S.Container>
  );
};
