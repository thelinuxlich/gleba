CREATE UNLOGGED TABLE "pessoas"(
    "id" varchar(36) NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
    "apelido" varchar(32) NOT NULL UNIQUE,
    "nome" varchar(100) NOT NULL,
    "nascimento" date NOT NULL,
    "stack" text DEFAULT '[]',
    "search" text GENERATED ALWAYS AS (LOWER(nome) || LOWER(apelido) || LOWER(stack)) STORED
);

CREATE EXTENSION IF NOT EXISTS pg_trgm SCHEMA pg_catalog;

CREATE INDEX CONCURRENTLY idx_pessoas_apelido_trg ON "pessoas" USING gist("search" gist_trgm_ops);

