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
-define(KPXML_B, 'http://karolis.5grupe.lt/biosensor/model/bound').

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
    {ok, {model, kpxml_v1,
        lists:map(
            fun (X) -> {substance, attr_value(X, name)} end,
            lists:filter(node_qn_pred({?KPXML_M, substance}), Contents)
        ),
        lists:map(
            fun (X) -> process_xml_kpxml_v1(reaction, X) end,
            lists:filter(node_qn_pred({?KPXML_M, reaction}), Contents)
        ),
        lists:map(
            fun (X) -> process_xml_kpxml_v1(medium, X) end,
            lists:filter(node_qn_pred({?KPXML_M, medium}), Contents)
        ),
        lists:map(
            fun (X) -> process_xml_kpxml_v1(bound, X) end,
            lists:filter(node_qn_pred({?KPXML_M, bound}), Contents)
        ),
        {transducer},
        lists:map(
            fun (X) -> process_xml_kpxml_v1(symbol, X) end,
            lists:filter(node_qn_pred({?KPXML_M, symbol}), Contents)
        ),
        {solver}
    }};
process_xml_kpxml_v1(reaction, E) ->
    case xsi_type(E) of
        {type, ?KPXML_R, "ReductionOxidation"} -> RType = reaction_ro;
        {type, ?KPXML_R, "MichaelisMenten"} -> RType = reaction_mm
    end,
    process_xml_kpxml_v1(RType, E);
process_xml_kpxml_v1(R = reaction_ro, E = #xmlElement{content = Contents}) ->
    {R,
        attr_value(E, name),
        attr_value(E, rate),
        lists:map(
            fun (S) -> {substrate, attr_value(S, name), attr_value(S, coefficient)} end,
            lists:filter(node_qn_pred({?KPXML_R, substrate}), Contents)
        ),
        lists:map(
            fun (P) -> {product, attr_value(P, name), attr_value(P, coefficient)} end,
            lists:filter(node_qn_pred({?KPXML_R, product}), Contents)
        )
    };
process_xml_kpxml_v1(R = reaction_mm, E) ->
    {R,
        attr_value(E, name),
        attr_value(E, substrate),
        attr_value(E, product),
        attr_value(E, "V_max"),
        attr_value(E, "K_M")
    };
process_xml_kpxml_v1(medium, E = #xmlElement{content = Contents}) ->
    {medium,
        attr_value(E, name),
        attr_value(E, diffusionRatio),
        lists:map(
            fun (S) -> {substance, attr_value(S, name), attr_value(S, diffusion), attr_value(S, initial)} end,
            lists:filter(node_qn_pred({?KPXML_M, substance}), Contents)
        ),
        lists:map(
            fun (S) -> {reaction, attr_value(S, name)} end,
            lists:filter(node_qn_pred({?KPXML_M, reaction}), Contents)
        ),
        lists:map(
            fun (S) -> {area,
                attr_value(S, top),
                attr_value(S, bottom),
                attr_value(S, left),
                attr_value(S, right),
                attr_value(S, from),
                attr_value(S, to)
            } end,
            lists:filter(node_qn_pred({?KPXML_M, area}), Contents)
        )
    };
process_xml_kpxml_v1(bound, E = #xmlElement{content = Contents}) ->
    {bound,
        attr_value(E, name),
        attr_value(E, from),
        attr_value(E, to),
        attr_value(E, at),
        lists:map(
            fun (S) ->
                case xsi_type(S) of
                    {type, ?KPXML_B, "Constant"} ->
                        {substance_const,
                            attr_value(S, name), attr_value(S, diffusion), attr_value(S, initial),
                            attr_value(S, concentration)
                        };
                    {type, ?KPXML_B, "Wall"} ->
                        {substance_wall,
                            attr_value(S, name), attr_value(S, diffusion), attr_value(S, initial)
                        };
                    {type, ?KPXML_B, "Merge"} ->
                        {substance_merge,
                            attr_value(S, name), attr_value(S, diffusion), attr_value(S, initial)
                        };
                    {type, ?KPXML_B, "Null"} ->
                        {substance_null,
                            attr_value(S, name), attr_value(S, diffusion), attr_value(S, initial)
                        }
                end
            end,
            lists:filter(node_qn_pred({?KPXML_M, substance}), Contents)
        ),
        lists:map(
            fun (S) -> {reaction, attr_value(S, name)} end,
            lists:filter(node_qn_pred({?KPXML_M, reaction}), Contents)
        )
    };
process_xml_kpxml_v1(symbol, E) ->
    {symbol,
        attr_value(E, name),
        attr_value(E, value),
        attr_value(E, dimension)
    }.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Helper functions.
%%

%%
%%  @doc Get attribute value of the specified xmlElement.
%%  @spec attr_value(XmlElement::#xmlElement, AttrName::atom()...) -> AttrValue::string()
%%
attr_value(#xmlElement{attributes = Attrs}, Name) ->
    L = lists:filter(
        fun (#xmlAttribute{expanded_name = N}) when N == Name -> true; (_) -> false end,
        Attrs
    ),
    if
        L == [] -> undefined;
        true -> [#xmlAttribute{value = Value}|_] = L, Value
    end.

%%
%%  @doc Resolves value of the xsi:type attribute.
%%  @spec xsi_type(Element) -> {type, Namespace, LocalName}
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

%%
%%  @doc Returns predicate to filter xml elements by qualified name.
%%  @spec node_qn_pred(QName) -> Fun.
%%
node_qn_pred(QName) ->
    fun (#xmlElement{expanded_name = N}) when N == QName -> true; (_) -> false end.


