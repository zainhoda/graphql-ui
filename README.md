# graphql-ui
Autogenerate a user interface (UI) for queries and mutations from a GraphQL schema using introspection

Reimplementation of https://github.com/4Catalyzer/graphql-explorer

# Goals
- No backend -- the only backend is the GraphQL endpoint
- Loadable via CDN `<script>` tag 
- Configurable using Elm flags https://guide.elm-lang.org/interop/flags.html

# TODO

## Startup
- [x] Specify a GraphQL endpoint in a JSON config
- [x] Read the JSON config into Elm
- [ ] Use introspection to get information on types, queries, and mutations

## Initial Models
- [ ] Model Types
- [ ] Model Queries
- [ ] Model Mutations

## Basic Functionality
- [ ] Hit GraphQL endpoint with a query using code from https://package.elm-lang.org/packages/ghivert/elm-graphql/latest/
- [ ] Store query results https://package.elm-lang.org/packages/andre-dietrich/elm-generic/latest/Generic#Value
- [ ] Display output

## Views
- [ ] Tables
- [ ] Forms
- [ ] Others?

## Linking UI Elements
- [ ] Fields -> DisplayView
- [ ] DisplayView -> FormView population
- [ ] FormView from Mutation Parameters
- [ ] Run mutation on FormView submit

## Advanced
- [ ] Verify config against GraphQL endpoint
