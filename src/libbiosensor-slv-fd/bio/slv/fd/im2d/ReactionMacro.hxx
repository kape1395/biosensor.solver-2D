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
