import { BrowserRouter, Route, Routes } from "react-router-dom";
import "./App.css";
import { BasePage } from "./pages/BasePage";
import { Home } from "./pages/Home";

const App = () => {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<BasePage />}>
          <Route path="/home" element={<Home />} />
        </Route>
      </Routes>
    </BrowserRouter>
  );
};

export default App;
