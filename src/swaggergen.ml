open Yojson

let string v = Some (`String v)
let ostring = function None -> None | Some v -> string v
let stringlist v = Some (`List (List.map (fun v -> `String v) v))
let ostringlist = function None -> None | Some v -> Some (`List (List.map (fun v -> `String v) v))
let olist = function None -> None | Some v -> Some (`List v)

let bool v = Some (`Bool v)
let obool = function None -> None | Some v -> bool v

let json (v : json) = Some v
let ojson (v : json option) = v
let ojsonlist : (json list option -> json option) = function None -> None | Some v -> Some (`List v)

type assoc = string * (json option)
type pre_object = assoc list
type json_object = (string * json) list

let assoc (lst : pre_object) : json =
	let rec loop = function
		| [] -> []
		| (name, Some v) :: tl -> (name, v) :: loop tl
		| (name, None) :: tl -> loop tl
	in
	let lst = loop lst in
	`Assoc lst

let oassoc (lst : pre_object) : json option = Some (assoc lst)

let contact name url email =
	assoc [
		"name", string name;
		"url", string url;
		"email", string email;
	]

let license ?url name =
	assoc [
		"name", string name;
		"url", ostring url;
	]

let info ?description ?termsOfService ?contact ?license title version =
	assoc [
		"title", string title;
		"description", ostring description;
		"termsOfService", ostring termsOfService;
		"contact", contact;
		"license", license;
		"version", string version;
	]

let parameter ?description ?required name _in (* TODO *) =
	let required = if _in = "path" then Some true else required in
	assoc [
		"name", string name;
		"in", string _in;
		"description", ostring description;
		"required", obool required;
	]

module Schema =
	struct
		type s_string = {
			string_format : string option;
		}

		type s_integer = {
			integer_format : string option;
		}

		type property = {
			name : string;
			content : t;
			required : bool;
		}

		and s_object = property list

		and t =
			| String of s_string
			| Integer of s_integer
			| Object of s_object

		let s_string ?s_format () = String { string_format = s_format }

		let s_integer ?s_format () = Integer { integer_format = s_format }

		let s_property ?(required=false) name content = { name; content; required }

		let s_object props = Object props

		let rec to_json = function
			| String v ->
				assoc [
					"type", string "string";
					"format", ostring v.string_format;
				]
			| Integer v ->
				assoc [
					"type", string "integer";
					"format", ostring v.integer_format;
				]
			| Object v ->
				let properties = List.map (fun v -> v.name, (to_json v.content |> json)) v |> assoc in
				let required = List.fold_left (fun ret v -> if v.required then v.name :: ret else ret) [] v |> stringlist in
				assoc [
					"type", string "object";
					"properties", json properties;
					"required", required;
				]
	end

let addresponse ?(codevalue=`Default) ?schema ?headers ?examples description lst =
	let v = assoc [
		"description", string description;
		"schema", ojson schema;
		"headers", ojson headers;
		"examples", ojson examples;
	] in
	let name = match codevalue with
		| `Default -> "default"
		| `Code v -> string_of_int v
	in
	(name, Some v) :: lst

let operation
	?tags
	?summary
	?description
	?externalDocs
	?operationId
	?consumes
	?produces
	?parameters
	?schemes
	?deprecated
	?security
	responses
	=
	assoc [
		"tags", ostringlist tags;
		"summary", ostring summary;
		"description", ostring description;
		"externalDocs", ojson externalDocs;
		"operationId", ostring operationId;
		"consumes", ostringlist consumes;
		"produces", ostringlist produces;
		"parameters", ojson parameters;
		"responses", oassoc responses;
		"schemes", ostringlist schemes;
		"deprecated", obool deprecated;
		"security", ojson security;
	]

let addpath path ?reference ?get ?put ?post ?delete ?options ?head ?patch ?parameters lst =
	let item = assoc [
		"$ref", ojson reference;
		"get", ojson get;
		"put", ojson put;
		"post", ojson post;
		"delete", ojson delete;
		"options", ojson options;
		"head", ojson head;
		"patch", ojson patch;
		"parameters", ojsonlist parameters;
	] in
	(path, Some item) :: lst

let swagger
	?host
	?basePath
	?schemes
	?consumes
	?produces
	?definitions
	?parameters
	?responses
	?securityDefinitions
	?security
	?tags
	?externalDocs
	info
	paths
	=
	assoc [
		"swagger", string "2.0";
		"info", json info;
		"host", ostring host;
		"basePath", ostring basePath;
		"schemes", ostringlist schemes;
		"consumes", ostringlist consumes;
		"produces", ostringlist produces;
		"paths", json paths;
		"definitions", ojson definitions;
		"parameters", ojson parameters;
		"responses", ojson responses;
		"securityDefinitions", ojson securityDefinitions;
		"security", olist security;
		"tags", olist tags;
		"externalDocs", ojson externalDocs;
	]
