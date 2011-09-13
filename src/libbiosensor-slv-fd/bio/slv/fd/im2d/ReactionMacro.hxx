/*
 * Copyright 2011 Karolis Petrauskas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * Implementation of the calculation of the reaction rate.
 * This part is implemented as macro for performance reasons.
 * Altrough I haven't tested performance differences between implementations...
 *
 * @param f_r variable to collect results.
 *      Results are added to this variable.
 *      It should be of type "double".
 * @param sourceLayer
 * @param dataHV
 * @param reactionsMM
 * @param reactionsMMPartCounts
 * @param reactionsRO
 * @param reactionsROPartCounts
 */
#define AREA_SUBSOLVER_REACTION_MACRO( \
    f_R, sourceLayer, dataHV, \
    reactionsMM, reactionsMMPartCounts, \
    reactionsRO, reactionsROPartCounts \
    ) \
    for (int r = reactionsMMPartCounts; r--; ) \
    { \
        ReactionMMPart mm = reactionsMM[r]; \
        f_R += (mm.V_max * dataHV[mm.substrateIndex][sourceLayer]) \
               / (mm.K_M + dataHV[mm.substrateIndex][sourceLayer]); \
    } \
    for (int r = reactionsROPartCounts; r--; ) \
    { \
        ReactionROPart ro = reactionsRO[r]; \
        f_R += ro.rate \
               * dataHV[ro.substrate1Index][sourceLayer] \
               * (ro.substrate2Index != -1 ? dataHV[ro.substrate2Index][sourceLayer] : 1.0); \
    }
