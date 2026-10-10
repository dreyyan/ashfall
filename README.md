# Ashfall

**Ashfall** is a post-apocalyptic robot colony simulator built with Haskell.

In a world devastated by catastrophe, autonomous robots must gather resources, manage energy, and maintain a colony to survive.

The project is currently under active development, beginning with a terminal-based simulation before expanding into 2D and potentially 3D graphics.

## Features

### Implemented
- Robot creation and status management
- Robot task assignment
- Energy consumption and recharging
- Resource types and storage management
- Robot inventory and carrying capacity
- Base storage and capacity management
- Resource transfers between robots and the base
- Error handling using `Either`
- Automated testing with Hspec

### Planned
- Mining and resource collection
- Exploration and robot repairs
- Simulation ticks and game state management
- Robot automation
- Interactive terminal interface (TUI)
- 2D graphical interface
- Potential 3D visualization

## Technology Stack

- **Language:** Haskell
- **Build System:** Cabal
- **Testing:** Hspec
- **Data Structures:** `Data.Map.Strict`
- **Planned Graphics:** Gloss / Raylib

## Project Structure

```text
src/
└── Ashfall/
    ├── Error.hs       # Game errors
    ├── Resource.hs    # Resource types and properties
    ├── Storage.hs     # Shared storage operations
    ├── Robot.hs       # Robot state and behavior
    ├── Base.hs        # Colony base management
    └── Transfer.hs    # Resource transfers
```

## Getting Started

### Prerequisites

- GHC
- Cabal

### Build

```bash
cabal build
```

### Run Tests

```bash
cabal test
```

## Development Roadmap

- [x] Robot and resource data models
- [x] Energy and task management
- [x] Inventory and storage systems
- [x] Resource transfers
- [ ] Mining and exploration
- [ ] Simulation engine
- [ ] Terminal-based gameplay
- [ ] Robot automation
- [ ] Save and load system
- [ ] 2D visualization
- [ ] 3D visualization

## Project Goals

Ashfall is a learning project focused on applying functional programming concepts to game development, including:

- Pure functions and immutable state
- Algebraic data types and pattern matching
- Error handling with `Maybe` and `Either`
- Modular software architecture
- Automated testing
- State management and simulation design

## Status

**Early Development — Core Systems**

The foundational robot, resource, and storage systems are implemented. Development is currently focused on introducing robot actions and building the simulation engine.