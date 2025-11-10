import { Link } from "react-router-dom";
import styled from "styled-components";

export const Container = styled.li``;

type Props = {
  $isThisPage: boolean;
};

export const StyledLink = styled(Link)<Props>`
  text-decoration: none;
  font-size: 24px;
  color: white;
  transition: 0.2s ease;
  border-bottom: ${(props) => (props.$isThisPage ? "3px solid white" : "none")};

  &:hover {
    opacity: 0.5;
  }
`;
