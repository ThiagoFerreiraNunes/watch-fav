import styled from "styled-components";

export const Container = styled.header`
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0px 40px;
  height: 90px;
  background-color: #00008b;

  > div {
    cursor: pointer;
    display: flex;
    align-items: center;
    gap: 5px;
  }

  > div h2 {
    font-size: 30px;
    margin-top: 5px;
    color: white;
  }

  > div img {
    width: 55px;
  }

  ul {
    display: flex;
    gap: 40px;
    list-style-type: none;
  }
`;
