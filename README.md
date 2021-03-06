# graphql-ui
Autogenerate a user interface (UI) for queries and mutations from a GraphQL schema using introspection

Inspired by https://github.com/4Catalyzer/graphql-explorer

Out of the box, all you have to do is specify a GraphQL endpoint.
- UI is generated automatically
- All queries and mutations show up as buttons
- All queries and mutations will display a form with required and optional arguments for the query/mutation
- No need to specify fields to get from each query/mutation -- all fields will be requested within a selected depth
- Query responses will be displayed in nested tables and can handle arbitrarily complicated nesting
- You can configure additional buttons to link fields in the query response to fields in new queries or mutations (i.e. you can specify a button to delete a row and have the id of the row passed to the delete mutation)

# Goals
- No backend -- the only backend is the GraphQL endpoint
- Loadable via CDN `<script>` tag 
- Configurable using Elm flags https://guide.elm-lang.org/interop/flags.html

# Usage
## Inside the head of your HTML
You are getting the stylesheet and the compiled JavaScript
```html
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css">
<script src="https://cdn.jsdelivr.net/gh/zainhoda/graphql-ui@0.1/output/graphql-ui.min.js"></script>
```

## Inside the body of your HTML
You just need to specify an HTML element where the app will live, set the configuration, and then you're good to go
```html
<div id="myapp"></div>
<script>
var app = Elm.Main.init({
node: document.getElementById('myapp'),
flags: 
    { 'graphqlEndpoint': '../query'
    , 'debugMode': true
    , 'buttonConfig': 
    [ { 'displayName': 'Delete Product'
        , 'context': 'products.data.products'
        , 'formToDisplay': 'removeProduct'
        , 'fields': 
        [ { 'inputField': 'id'
            , 'formField': 'removeProduct.input'
            }
        ]
        }
    , { 'displayName': 'Get Targets'
        , 'context': 'products.data.products'
        , 'formToDisplay': 'productTargets'
        , 'fields': 
        [ { 'inputField': 'id'
            , 'formField': 'productTargets.id'
            }
        ]
        }
    ]
    }
});
```

# Roadmap

## 0.1
- [x] Specify a GraphQL endpoint in a JSON config
- [x] Read the JSON config into Elm
- [x] Store introspection query as a const
- [x] Send introspection query in query body
- [x] Use the decoder from https://github.com/dillonkearns/elm-graphql/blob/master/generator/src/Graphql/Parser.elm#L13-L15
- [x] Model Types
- [x] Model Queries
- [x] Model Mutations
- [x] Hit GraphQL endpoint with a query using code from https://package.elm-lang.org/packages/ghivert/elm-graphql/latest/
- [x] List Queries
- [x] List Mutations
- [x] Forms from Query/Mutation Arguments
- [x] Store form values in a Dict QueryName (Dict FieldName String)
- [x] Form fields to GraphQL Query/Mutation
- [x] Add most of the fields to the request
- [x] Store query results https://package.elm-lang.org/packages/andre-dietrich/elm-generic/latest/Generic#Value
- [x] Tables/FieldViewer from Query Results
- [x] argToFormField - lookup enum values (show fields for any kind of TypeReference actually)
- [x] inputScalarOrEnum should be renamed to inputFromTypeRef
- [x] Move case Type.InputObjectRef objectClassCaseName to inputFromTypeRef
- [x] Currently set value needs to be displayed in the form input
- [x] argToFormField - send additional type information to UpdateFormInput
- [x] Handle enum types instead of just string (i.e. GraphQl.string) 
- [x] UnionRef
- [x] Displaying UnionRef tables accounts for all fields first when constructing the header of the tables and then displaying the rows should be based on a list of fields
- [x] Mutations
- [x] Change Modal to a whole page
- [x] Make the URL editable
- [x] Handle Mutations
- [x] Use the nullability when displaying the form field
- [x] Use the nullability when inputting the form field
- [x] Use the nullability when making the GraphQL query
- [x] Use RemoteData package https://package.elm-lang.org/packages/krisajenkins/remotedata/latest/
- [x] Get rid of config.json and instead pass it through flags so that it's runtime-configurable
- [x] Use GraphQL Descriptions in Form Inputs and maybe Table tooltips
- [x] Pass button config to the generic views
- [x] When the current path matches the context, send the button_config and the the current data to the Msg 
- [x] Msg should run a series of updateFormAt, and then set the active form 
- [x] Read in configuration from Flags

## 0.2
- [ ] Prettify Camel Case, Snake Case, and all Caps to more human-readable output via a function 
- [ ] Implement getArgumentTypeAt
- [ ] typeRefToArgumentType - handle all types
- [ ] inputFromTypeRef - Handle all types
- [ ] Handle Lists in QueryArgument
- [ ] Use the typeRef of the argument for displaying the form field
- [ ] Use the typeRef of the argument for storing the form field

## 0.3
- [ ] Don't allow "Run Query" button to be clickable until all required arguments are met
- [ ] EverySet type?
- [ ] queryArgumentToGraphQlAgument - handle other types of lists
### Edit View
- [ ] Config flag to set edit_mode = true
- [ ] Display the JSON of the config in edit mode
- [ ] On edit mode, certain config fields are editable directly

## 0.4
- [ ] Verify config against GraphQL endpoint
