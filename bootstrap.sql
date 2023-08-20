CREATE UNLOGGED TABLE "pessoas"(
    "id" varchar(36) NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
    "apelido" varchar(32) NOT NULL UNIQUE,
    "nome" varchar(100) NOT NULL,
    "nascimento" date NOT NULL,
    "stack" text DEFAULT '[]'
);

INSERT INTO pessoas(apelido, nome, nascimento, stack)
    VALUES ('jose', 'Jose da Silva', '1990-01-01', '["java", "python"]');

CREATE EXTENSION IF NOT EXISTS pg_trgm SCHEMA pg_catalog;

CREATE INDEX idx_pessoas_apelido_trgm ON "pessoas" USING gin("apelido" gin_trgm_ops);

CREATE INDEX idx_pessoas_nome_trgm ON "pessoas" USING gin("nome" gin_trgm_ops);

CREATE INDEX idx_pessoas_nome_trgm ON "stack" USING gin("stack" gin_trgm_ops);

