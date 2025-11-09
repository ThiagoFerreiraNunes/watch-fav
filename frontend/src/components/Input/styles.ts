import styled from "styled-components";

export const Container = styled.div`
  display: flex;
  align-items: center;
  background-color: #e7e6e6ff;
  width: 600px;
  height: 40px;
  border-radius: 10px;
  box-shadow: 0px 5px 5px gray;
  gap: 5px;

  input {
    border: none;
    background-color: inherit;
    outline: none;
    width: 100%;
    padding-left: 10px;
    font-size: 24px;
  }

  > div {
    display: flex;
    align-items: center;
    justify-content: center;
    transition: 0.2s ease;
    cursor: pointer;
    height: 100%;
    width: 50px;
    background-color: #1c1c1c;
    border-radius: 0px 10px 10px 0px;

    &:hover {
      opacity: 0.8;
    }
  }
`;
