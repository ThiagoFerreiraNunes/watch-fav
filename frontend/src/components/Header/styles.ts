import styled from "styled-components";

export const Container = styled.header`
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0px 40px;
  height: 60px;
  background-color: #1c1c1c;

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
    width: 40px;
  }

  ul {
    display: flex;
    gap: 40px;
    list-style-type: none;
  }
`;
