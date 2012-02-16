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
-define(KPXML_M, 'http://karolis.5grupe.lt/biosensor/model').
-define(KPXML_R, 'http://karolis.5grupe.lt/biosensor/model/reaction').

%%
%%  Convert XML structure to the model definition.
%%
process_xml(#xmlDocument{content = [E|[]]}) ->
    process_xml(E);
process_xml(E = #xmlElement{expanded_name = {?KPXML_M, model}}) ->
    process_xml_kpxml_v1(root, E);
process_xml(E) ->
    {error, "Unknown element", E}.


%%
%%  Converts documents of type {http://karolis.5grupe.lt/biosensor/model}model
%%  to the corresponding model definition.
%%
process_xml_kpxml_v1(root, #xmlElement{expanded_name = {?KPXML_M, model}, content = Contents}) ->
    ElemIsSubst = fun (#xmlElement{expanded_name = {?KPXML_M, substance}}) -> true; (_) -> false end,
    ElemIsReact = fun (#xmlElement{expanded_name = {?KPXML_M, reaction}}) -> true; (_) -> false end,
    {ok, {model, kpxml_v1,
        lists:map(
            fun (X) -> {substance, attr_value(X, name)} end,
            lists:filter(ElemIsSubst, Contents)
        ),
        lists:map(
            fun (X) -> process_xml_kpxml_v1(reaction, X) end,
            lists:filter(ElemIsReact, Contents)
        ), 
        [], 
        []
    }};
process_xml_kpxml_v1(reaction, E) ->
    case xsi_type(E) of
        {type, ?KPXML_R, "ReductionOxidation"} -> RType = reaction_ro;
        {type, ?KPXML_R, "MichaelisMenten"} -> RType = reaction_mm
    end,
    process_xml_kpxml_v1(RType, E);
process_xml_kpxml_v1(R = reaction_ro, E = #xmlElement{content = Contents}) ->
    ElemIsS = fun (#xmlElement{expanded_name = {?KPXML_R, substrate}}) -> true; (_) -> false end,
    ElemIsP = fun (#xmlElement{expanded_name = {?KPXML_R, product}}) -> true; (_) -> false end,
    {R,
        attr_value(E, name),
        attr_value(E, rate),
        lists:map(fun (S) -> {substrate, attr_value(S, name)} end, lists:filter(ElemIsS, Contents))
        ++
        lists:map(fun (P) -> {product, attr_value(P, name)} end, lists:filter(ElemIsP, Contents))
    };
process_xml_kpxml_v1(R = reaction_mm, E) ->
    {R,
        attr_value(E, name),
        attr_value(E, substrate),
        attr_value(E, product),
        attr_value(E, "V_max"),
        attr_value(E, "K_M")
    }.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Helper functions.
%%

%%
%%  @doc Get attribute value of the specified xmlElement.
%%  @spec attr_value(XmlElement::#xmlElement, AttrName::atom()...) -> AttrValue::string()
%%
attr_value(#xmlElement{attributes = Attrs}, Name) ->
    [#xmlAttribute{value = Value}|_] = lists:filter(
        fun (#xmlAttribute{expanded_name = N}) when N == Name -> true; (_) -> false end,
        Attrs
    ),
    Value.

%%
%%  @doc Resolves value of the xsi:type attribute.
%%
xsi_type(#xmlElement{attributes = Attrs}) ->
    [#xmlAttribute{namespace=#xmlNamespace{default = DefNs, nodes = Ns}, value = Val}|_] = lists:filter(
        fun (#xmlAttribute{expanded_name = {'http://www.w3.org/2001/XMLSchema-instance',type}}) -> true; (_) -> false end,
        Attrs
    ),
    [T1|T2] = string:tokens(Val, ":"),
    if
        T2 == [] ->
            {type, DefNs, T1};
        true ->
            [TypeNs|_] = [NsValue || {NsPrefix, NsValue} <- Ns, NsPrefix == T1],
            [TypeName|_] = T2,
            {type, TypeNs, TypeName}
    end.


