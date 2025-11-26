import { BrowserRouter, Route, Routes } from "react-router-dom";
import "./App.css";
import { BasePage } from "./pages/BasePage";
import { Home } from "./pages/Home";
import { Movies } from "./pages/Movies";
import { Series } from "./pages/Series";
import { Suggestions } from "./pages/Suggestions";
import { Login } from "./pages/Login";
import { Register } from "./pages/Register";
import { SpecificMovie } from "./pages/SpecificMovie";

const App = () => {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/login" element={<Login />} />
        <Route path="/register" element={<Register />} />

        <Route path="/" element={<BasePage />}>
          <Route path="/home" element={<Home />} />
          <Route path="/movies" element={<Movies />} />
          <Route path="/movies/:id" element={<SpecificMovie />} />
          <Route path="/series" element={<Series />} />
          <Route path="/suggestions" element={<Suggestions />} />
        </Route>
      </Routes>
    </BrowserRouter>
  );
};

export default App;
