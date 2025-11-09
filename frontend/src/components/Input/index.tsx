import * as S from "./styles";
import { FaSearch } from "react-icons/fa";

type Props = {
  placeholder: string;
};

export const Input = ({ placeholder }: Props) => {
  return (
    <S.Container>
      <input type="search" placeholder={placeholder} />
      <div>
        <FaSearch color="white" size={24} />
      </div>
    </S.Container>
  );
};
