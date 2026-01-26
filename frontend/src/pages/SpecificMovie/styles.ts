import styled from "styled-components";

export const Container = styled.section`
  background-color: white;
  padding: 40px 40px;
  min-height: calc(100vh - 100px);
  display: flex;
  justify-content: center;

  > div {
    background-color: #e7e6e6ff;
    box-shadow: 0px 0px 20px gray;
    border-radius: 20px;
    padding: 20px 30px;
    display: flex;
    flex-direction: column;
    gap: 20px;
    position: relative;

    .back-icon {
      font-size: 50px;
      position: absolute;
      cursor: pointer;
      transition: 0.2s ease;
      background-color: inherit;
      border-radius: 100%;

      &:hover {
        background-color: gray;
      }
    }

    > aside {
      text-align: center;

      > h1 {
        font-size: 40px;
        margin-bottom: 20px;
      }

      > img {
        width: 1200px;
        max-width: 1200px;
        border-radius: 20px;
      }
    }

    > main {
      width: 1200px;
      max-width: 1200px;
      display: flex;
      flex-direction: column;
      gap: 10px;

      > h2 {
        font-size: 36px;
        text-align: center;
        text-decoration: underline;
      }
    }
  }
`;
