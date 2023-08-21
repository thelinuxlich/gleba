CREATE UNLOGGED TABLE "pessoas"(
    "id" varchar(36) NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
    "apelido" varchar(32) NOT NULL UNIQUE,
    "nome" varchar(100) NOT NULL,
    "nascimento" date NOT NULL,
    "stack" text DEFAULT '[]',
    "search" text GENERATED ALWAYS AS (LOWER(nome) || LOWER(apelido) || LOWER(stack)) STORED
);

INSERT INTO pessoas(apelido, nome, nascimento, stack)
    VALUES ('jose', 'Jose da Silva', '1990-01-01', '["java", "python"]');

CREATE EXTENSION IF NOT EXISTS pg_trgm SCHEMA pg_catalog;

CREATE INDEX idx_pessoas_apelido_trg ON "pessoas" USING gin("search" gin_trgm_ops);

