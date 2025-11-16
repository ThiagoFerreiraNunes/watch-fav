import { Link } from "react-router-dom";
import styled from "styled-components";

export const StyledLink = styled(Link)`
  font-weight: 700;
  text-decoration: none;
  border: 2px solid white;
  border-radius: 10px;
  padding: 5px;
  color: white;
  transition: 0.2s ease;
  font-size: 24px;

  &:hover {
    opacity: 0.6;
  }
`;
