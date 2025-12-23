# ⚽ Player Finishing Analysis Dashboard

An interactive R Shiny application for analyzing football player finishing profiles using Expected Goals (xG) data from Understat.

**🚀 [Live Demo](https://shivamsingh.shinyapps.io/player-finishing-analysis/)**

---

## 📊 Features

### Interactive Visualizations
- **📈 Line Chart**: Track Goals - xG performance over time with customizable rolling averages
- **🎯 Shot Maps**: Multiple visualization styles (Point, Hexbin, Density, Goal Zone Analysis, Shot Type Breakdown)
- **📊 Histogram**: Distribution of shot quality (xG values)

### Powerful Filtering
- Filter by season (2014-2024)
- Shot type (Left Foot, Right Foot, Header, Other)
- Situation (Open Play, Set Pieces, Penalties, etc.)
- Show goals only, misses only, or all shots

### Export Options
- 📥 Download plots as PNG, SVG, or PDF
- 📄 Export filtered data as CSV
- 🎨 Dark/Light theme toggle

### Summary Statistics
Real-time calculation of:
- Total shots and goals
- Expected Goals (xG)
- Conversion rate
- Average xG per shot
- Performance metric (Goals - xG)

---

## 📊 Data

The application uses shot-level data from [Understat](https://understat.com), covering:
- **Leagues**: Premier League, La Liga, Bundesliga, Serie A, Ligue 1
- **Seasons**: 2014/15 to 2024/25
- **Metrics**: Shot location (x, y), xG value, outcome, shot type, situation

---

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## 📸 Screenshots

### 🎯 Shot Map – Point Visualization (Dark Theme)
![Point Shot Map Dark](screenshots/Lionel_Messi_point.png)

### 🎯 Shot Map – Point Visualization (Light Theme)
![Point Shot Map Light](screenshots/Lionel_Messi_pointlight.png)

### 🗺️ Shot Map – Goal Zone Analysis (Dark Theme)
![Goal Zone Shot Map Dark](screenshots/Lionel_Messi_zonedark.png)

### 🗺️ Shot Map – Goal Zone Analysis (Light Theme)
![Goal Zone Shot Map Light](screenshots/Lionel_Messi_zonelight.png)

### 📊 Full Dashboard Overview
![Dashboard Overview](screenshots/Lionel_Messi_Dashboard.png)

---
