import styled from "styled-components";

export const Container = styled.section`
  padding: 40px 40px;
  display: flex;
  gap: 110px;
  justify-content: space-between;
  align-items: center;
  background-color: white;
  height: calc(100vh - 100px);

  > main {
    width: 50%;
    display: flex;
    flex-direction: column;
    gap: 20px;

    h1 {
      font-size: 40px;
    }

    div {
      display: flex;
      flex-direction: column;
      gap: 10px;
      text-align: justify;

      p {
        font-size: 24px;
      }
    }
  }

  > aside {
    width: 100%;
    width: fit-content;

    img {
      width: 600px;
    }
  }
`;
