open Yojson

type 'a t = Yojson.json

type path = string * [ `Path ] t option

type contact = [ `Contact ] t

type license = [ `License ] t

type info = [ `Info ] t

type parameter = [ `Parameter ] t

type parameters = [ `Parameter | `Reference ] t list

type schema = [ `Schema ] t

type response = [ `Response ] t

type externalDocs = [ `ExternalDocs ] t

type security = [ `Security ] t

type headers = [ `Headers ] t

type responseExamples = [ `ResponseExamples ] t

type operation = [ `Operation ] t

type definition = [ `Definition ] t

type tag = [ `Tag ] t

type securityScheme = [ `SecurityScheme ] t

type swagger = [ `Swagger ] t



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
type json_object = (string * json) list

let make_assoc (lst : assoc list) : json =
	let rec loop = function
		| [] -> []
		| (name, Some v) :: tl -> (name, v) :: loop tl
		| (name, None) :: tl -> loop tl
	in
	let lst = loop lst in
	`Assoc lst

let assoc (lst : assoc list) : json option = Some (make_assoc lst)
let oassoc = function | None -> None | Some v -> assoc v

let contact name url email =
	make_assoc [
		"name", string name;
		"url", string url;
		"email", string email;
	]

let license ?url name =
	make_assoc [
		"name", string name;
		"url", ostring url;
	]

let info ?description ?termsOfService ?(contact : contact option) ?(license : license option) title version =
	make_assoc [
		"title", string title;
		"description", ostring description;
		"termsOfService", ostring termsOfService;
		"contact", contact;
		"license", license;
		"version", string version;
	]

let parameter ?description ?required name _in (* TODO *) =
	let required = if _in = "path" then Some true else required in
	make_assoc [
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
		let string_defaults = {
			string_format = None;
		}

		type s_integer = {
			integer_format : string option;
		}
		let integer_defaults = {
			integer_format = None;
		}

		type property = {
			name : string;
			property_content : st;
			required : bool;
		}

		and s_object = {
			props : property list;
		}

		and s_array = {
			array_content : st;
		}

		and _type =
			| String of s_string
			| Integer of s_integer
			| Object of s_object
			| Array of s_array

		and st = {
			title : string option;
			description : string option;
			content : _type;
		}

		let defaults ~content ?title ?description () = {
			title;
			description;
			content;
		}

		let s_string ?(content=string_defaults) = defaults ~content:(String content)
		let s_integer ?(content=integer_defaults) = defaults ~content:(Integer content)

		let s_property ?(required=true) name content = { name; property_content = content; required }

		let s_object props = defaults ~content:(Object { props })
		let s_array array_content = defaults ~content:(Array { array_content })

		let _common v = [
			"title", ostring v.title;
			"description", ostring v.description;
		]

		let rec to_schema v =
			let vals = match v.content with
				| String c ->
					[
						"type", string "string";
						"format", ostring c.string_format;
					]
				| Integer c ->
					[
						"type", string "integer";
						"format", ostring c.integer_format;
					]
				| Object c ->
					let properties = List.map (fun p -> p.name, (to_schema p.property_content |> json)) c.props |> make_assoc in
					let required = List.fold_left (fun ret p -> if p.required then p.name :: ret else ret) [] c.props |> stringlist in
					[
						"type", string "object";
						"properties", json properties;
						"required", required;
					]
				| Array c ->
					[
						"type", string "array";
						"items", json (to_schema c.array_content);
					]
			in
			make_assoc (vals @ _common v)
	end

let addresponse
	?(code=`Default)
	?(schema : schema option)
	?(headers : headers option)
	?(examples : responseExamples option)
	description
	lst =
	let v = make_assoc [
		"description", string description;
		"schema", ojson schema;
		"headers", ojson headers;
		"examples", ojson examples;
	] in
	let name = match code with
		| `Default -> "default"
		| `Code v -> string_of_int v
	in
	(name, Some v) :: lst

let operation
	?tags
	?summary
	?description
	?(externalDocs : externalDocs option)
	?operationId
	?consumes
	?produces
	?(parameters : parameters option)
	?schemes
	?deprecated
	?(security : security option)
	(responses : (string * response option) list) : operation
	=
	make_assoc [
		"tags", ostringlist tags;
		"summary", ostring summary;
		"description", ostring description;
		"externalDocs", ojson externalDocs;
		"operationId", ostring operationId;
		"consumes", ostringlist consumes;
		"produces", ostringlist produces;
		"parameters", ojsonlist parameters;
		"responses", assoc responses;
		"schemes", ostringlist schemes;
		"deprecated", obool deprecated;
		"security", ojson security;
	]

let addpath
	path
	?reference
	?(get : operation option)
	?(put : operation option)
	?(post : operation option)
	?(delete : operation option)
	?(options : operation option)
	?(head : operation option)
	?(patch : operation option)
	?(parameters : parameters option)
	(lst : path list) : path list =
	let item = make_assoc [
		"$ref", ostring reference;
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
	?(definitions : definition list option)
	?(parameters : parameters option)
	?(responses : (string * ([ `Response | `Definition ] t option)) list option)
	?(securityDefinitions : (string * securityScheme option) list option)
	?(security : security list option)
	?(tags : tag list option)
	?(externalDocs : externalDocs option)
	(info : info)
	(paths : path list)
	=
	make_assoc [
		"swagger", string "2.0";
		"info", json info;
		"host", ostring host;
		"basePath", ostring basePath;
		"schemes", ostringlist schemes;
		"consumes", ostringlist consumes;
		"produces", ostringlist produces;
		"paths", assoc paths;
		"definitions", ojsonlist definitions;
		"parameters", ojsonlist parameters;
		"responses", oassoc responses;
		"securityDefinitions", oassoc securityDefinitions;
		"security", olist security;
		"tags", olist tags;
		"externalDocs", ojson externalDocs;
	]

let to_json t = t
