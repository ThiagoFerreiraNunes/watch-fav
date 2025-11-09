import styled from "styled-components";

export const Container = styled.main`
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
  flex-wrap: wrap;
  gap: 20px;
  width: 100%;
`;
