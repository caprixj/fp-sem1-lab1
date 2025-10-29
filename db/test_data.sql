INSERT OR IGNORE INTO resource_types (name) VALUES
  ('journal_article'), ('conference_paper'), ('thesis'), ('technical_report');

INSERT INTO authors (full_name, dept) VALUES
  ('Prof. Eleanor Vance','Physics'),
  ('Dr. Kenji Tanaka','Computer Science'),
  ('Dr. Maria Rodriguez','Biology'),
  ('Prof. Bob Brown','Math');

INSERT OR IGNORE INTO users (username, email, role) VALUES
  ('s.johnson','s.johnson@example.com','student'),
  ('m.chen','m.chen@example.com','staff'),
  ('sys_admin','admin@example.com','admin');

INSERT INTO resources (name, type_id, content, purpose, opened_at)
VALUES
  ('Advanced Haskell Techniques', 1, 'A deep dive into type families and GADTs', 'research', '2023-10-01'),
  ('ML Model for Protein Folding', 2, 'A novel approach using transformers for protein structure prediction.', 'research', '2024-01-15'),
  ('History of Computing Thesis', 3, 'Full text of the masters thesis on ENIAC.', 'education', '2023-05-20'),
  ('Quantum Computing Simulation Report', 4, 'Internal report on qubit simulation stability.', 'internal', '2024-02-01');

INSERT INTO resource_authors (resource_id, author_id)
VALUES
  (1, 2),
  (2, 2),
  (2, 3),
  (3, 4),
  (4, 1);

INSERT INTO usage_events (resource_id, user_id, action)
VALUES
  (1, 1, 'view'),
  (1, 1, 'download'),
  (2, 1, 'view'),
  (2, 2, 'view'),
  (2, 2, 'like'),
  (3, 1, 'download'),
  (1, 3, 'bookmark');
  