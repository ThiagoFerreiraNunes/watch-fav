import styled from "styled-components";

export const Container = styled.section`
  width: 100%;
  height: 100vh;
  display: flex;

  > main {
    width: 70%;
    background-color: white;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  > aside {
    width: 30%;
    background-color: #1c1c1c;
    display: flex;
    align-items: center;
    justify-content: center;

    > div {
      color: white;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      gap: 10px;

      > h2 {
        font-size: 30px;
      }

      > p {
        font-size: 20px;
      }
    }
  }
`;
