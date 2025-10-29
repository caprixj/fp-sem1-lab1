CREATE TABLE IF NOT EXISTS resource_types (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS resources (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  type_id INT NOT NULL,
  content TEXT,
  purpose TEXT,
  opened_at DATE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (type_id) REFERENCES resource_types(id)
);

CREATE TABLE IF NOT EXISTS authors (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  full_name TEXT NOT NULL,
  dept TEXT
);

CREATE TABLE IF NOT EXISTS resource_authors (
  resource_id INT NOT NULL,
  author_id INT NOT NULL,
  PRIMARY KEY (resource_id, author_id),
  FOREIGN KEY (resource_id) REFERENCES resources(id) ON DELETE CASCADE,
  FOREIGN KEY (author_id) REFERENCES authors(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS users (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT NOT NULL UNIQUE,
  email TEXT,
  role TEXT DEFAULT 'student' CHECK(role IN ('student','staff','admin'))
);

CREATE TABLE IF NOT EXISTS usage_events (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  resource_id INT NOT NULL,
  user_id INT,
  event_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  action TEXT NOT NULL CHECK(action IN ('view','download','like','bookmark')),
  FOREIGN KEY (resource_id) REFERENCES resources(id) ON DELETE CASCADE,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE SET NULL
);
