import styled from "styled-components";

export const Container = styled.div`
  display: flex;
  gap: 5px;
  align-items: center;
  justify-content: center;
  background-color: #e7e6e6ff;
  padding: 5px;
  border-radius: 5px;
  width: 100%;

  > input {
    background-color: transparent;
    border: none;
    font-size: 30px;
    outline: none;
    width: 90%;
  }
`;
