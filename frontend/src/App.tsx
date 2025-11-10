import { BrowserRouter, Route, Routes } from "react-router-dom";
import "./App.css";
import { BasePage } from "./pages/BasePage";
import { Home } from "./pages/Home";
import { Movies } from "./pages/Movies";
import { Series } from "./pages/Series";
import { Suggestions } from "./pages/Suggestions";

const App = () => {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<BasePage />}>
          <Route path="/home" element={<Home />} />
          <Route path="/movies" element={<Movies />} />
          <Route path="/series" element={<Series />} />
          <Route path="/suggestions" element={<Suggestions />} />
        </Route>
      </Routes>
    </BrowserRouter>
  );
};

export default App;
