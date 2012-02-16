%
% Copyright 2012 Karolis Petrauskas
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
-module(bio_ers_model).
-export([parse_file/1]).
-include_lib("xmerl/include/xmerl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Public functions.
%%

%%
%%  @doc Parses biosensor model file and returns corresponding model definition.
%%  @spec parse_file(FileName::string()) -> Model::tuple()
%%
parse_file(FileName) ->
    {Document, []} = xmerl_scan:file(FileName, [
        {space, normalize},
        {namespace_conformant, true},
        {comments, false},
        {document, true}
    ]),
    process_xml(Document).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Private functions.
%%

%%
%%  Convert XML structure to the model definition.
%%
process_xml(#xmlDocument{content = [E|[]]}) ->
    process_xml(E);
process_xml(E = #xmlElement{expanded_name = {'http://karolis.5grupe.lt/biosensor/model',model}}) ->
    process_xml_kpxml_v1(root, E);
process_xml(E) ->
    {error, "Unknown element", E}.


%%
%%  Converts documents of type {http://karolis.5grupe.lt/biosensor/model}model
%%  to the corresponding model definition.
%%
process_xml_kpxml_v1(root, #xmlElement{expanded_name = {'http://karolis.5grupe.lt/biosensor/model',model}, content = Contents}) ->
    ElemIsSubst = fun (#xmlElement{expanded_name = {'http://karolis.5grupe.lt/biosensor/model',substance}}) -> true; (_) -> false end,
    {ok, {model, kpxml_v1,
        [], 
        lists:map(fun (X) -> {substance, attr_value(X, name)} end, lists:filter(ElemIsSubst, Contents)),
        [], 
        [], 
        []
    }}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Helper functions.
%%

%%
%%  @doc Get attribute value of the specified xmlElement.
%%  @spec attr_value(XmlElement::#xmlElement, AttrName::atom()) -> AttrValue::string()
%%
attr_value(#xmlElement{attributes = Attrs}, Name) ->
    [#xmlAttribute{value = Value}|_] = lists:filter(
        fun (#xmlAttribute{name = N}) when N == Name -> true; (_) -> false end,
        Attrs
    ),
    Value.


