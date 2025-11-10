import styled from "styled-components";

export const Container = styled.div`
  display: flex;
  flex-direction: column;
  padding: 10px;
  background-color: #1c1c1c;
  border-radius: 10px;
  cursor: pointer;
  color: white;
  width: 250px;

  transition: 0.2s ease;

  &:hover {
    opacity: 0.9;
    transform: scale(1.02);
    box-shadow: 0px 0px 10px #1c1c1c;
  }

  img {
    height: 120px;
    width: 100%;
    border-radius: 5px;
  }

  h2 {
    font-size: 16px;
    font-weight: 700;
    margin-bottom: 10px;
    margin-top: 5px;
  }
`;
