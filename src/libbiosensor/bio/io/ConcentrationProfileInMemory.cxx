#include "ConcentrationProfileInMemory.hxx"
#include "../Logging.hxx"
#define LOGGER "libbiosensor::ConcentrationProfileInMemory: "

/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileInMemory::ConcentrationProfileInMemory(
    BIO_SLV_NS::ISolver* solver
)
{
    this->hasState = false;

    this->solver = solver;
    this->iterativeSolver = dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver);
    if (!iterativeSolver)
    {
        throw new BIO_NS::Exception("AdjustTimeStepAdaptively: solver must implement IIterativeSolver");
    }
    if ((this->solverData = dynamic_cast<BIO_DM_NS::IGrid2D*>(solver->getData())) == 0)
    {
        throw Exception("ConcentrationProfileInMemory: IGrid2D DataModel is required");
    }

    sizeS = solverData->getSubstanceCount();
    sizeH = solverData->getPointPositionsH()->getLength();
    sizeV = solverData->getPointPositionsV()->getLength();

    matrix = new double**[sizeH];
    for (int h = 0; h < sizeH; h++)
    {
        matrix[h] = new double*[sizeV];
        for (int v = 0; v < sizeV; v++)
        {
            matrix[h][v] = new double[sizeS];
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileInMemory::~ConcentrationProfileInMemory()
{
    for (int h = 0; h < sizeH; h++)
    {
        for (int v = 0; v < sizeV; v++)
        {
            delete[] matrix[h][v];
        }
        delete[] matrix[h];
    }
    delete[] matrix;
}


/* ************************************************************************** */
/* ****************    ISolverStateHolder   ********************************* */
/* ************************************************************************** */
BIO_SLV_NS::ISolverState* BIO_IO_NS::ConcentrationProfileInMemory::getSolverState()
{
    return this;
}


/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfileInMemory::setSolverState(BIO_SLV_NS::ISolverState* state)
{
    iterationNumber = state->getIteration();
    solvedTime = state->getTime();
    setState(state->getData());
    hasState = true;
}


/* ************************************************************************** */
bool BIO_IO_NS::ConcentrationProfileInMemory::hasSolverState()
{
    return hasState;
}


/* ************************************************************************** */
/* ****************    ISolverListener   ************************************ */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfileInMemory::solveEventOccured()
{
    iterationNumber = iterativeSolver->getSolvedIterationCount();
    solvedTime = iterativeSolver->getSolvedTime();
    setState(solver->getData());
    hasState = true;
}


/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfileInMemory::reset()
{
    hasState = false;
}


/* ************************************************************************** */
/* ****************    ISolverState   *************************************** */
/* ************************************************************************** */
double BIO_IO_NS::ConcentrationProfileInMemory::getTime()
{
    if (!hasState)
        throw BIO_NS::Exception("Time is not defined...");
    return solvedTime;
}


/* ************************************************************************** */
long BIO_IO_NS::ConcentrationProfileInMemory::getIteration()
{
    if (!hasState)
        throw BIO_NS::Exception("Iteration is not defined...");
    return iterationNumber;
}


/* ************************************************************************** */
BIO_DM_NS::IDataModel* BIO_IO_NS::ConcentrationProfileInMemory::getData()
{
    return this;
}


/* ************************************************************************** */
/* ****************    IDataModel   ***************************************** */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfileInMemory::setState(BIO_DM_NS::IDataModel *source)
{
    if (source != solver->getData())
    {
        LOG_WARN(LOGGER
                 << "setState: Source data will be ignored."
                 << " Specified solver will be used instead."
                );
    }
    BIO_DM_NS::ICursor2D* cursor = solverData->newGridCursor();

    int h;
    int v;
    cursor->colStart();
    cursor->rowStart();
    for (v = 0; cursor->isValid(); v++, cursor->down())
    {
        for (h = 0; cursor->isValid(); h++, cursor->right())
        {
            BIO_DM_NS::IConcentrations* concentrations = cursor->getConcentrations();
            for (int s = 0; s < sizeS; s++)
            {
                matrix[h][v][s] = concentrations->getConcentration(s);
            }
        }
        cursor->rowStart();
    }

    delete cursor;
}


/* ************************************************************************** */
/* ****************    IGrid2D   ******************************************** */
/* ************************************************************************** */
BIO_DM_NS::ISegmentSplit* BIO_IO_NS::ConcentrationProfileInMemory::getPointPositionsH()
{
    return solverData->getPointPositionsH();
}

/* ************************************************************************** */
BIO_DM_NS::ISegmentSplit* BIO_IO_NS::ConcentrationProfileInMemory::getPointPositionsV()
{
    return solverData->getPointPositionsV();
}

/* ************************************************************************** */
BIO_DM_NS::ICursor2D* BIO_IO_NS::ConcentrationProfileInMemory::newGridCursor()
{
    return new Cursor2DImpl(this);
}

/* ************************************************************************** */
int BIO_IO_NS::ConcentrationProfileInMemory::getSubstanceCount()
{
    return solverData->getSubstanceCount();
}

/* ************************************************************************** */
BIO_XML_NS::model::Substance* BIO_IO_NS::ConcentrationProfileInMemory::getSubstanceConf(int index)
{
    return solverData->getSubstanceConf(index);
}


/* ************************************************************************** */
/* ****************    ConcentrationProfileInMemory::Cursor2DImpl    ******** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileInMemory::Cursor2DImpl::Cursor2DImpl(ConcentrationProfileInMemory* source) :
        BIO_DM_NS::AbstractCursor2D(
            source->sizeH,
            source->sizeV
        )
{
    this->source = source;
}


/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileInMemory::Cursor2DImpl::~Cursor2DImpl()
{
    // Nothing
}


/* ************************************************************************** */
BIO_DM_NS::IConcentrations *BIO_IO_NS::ConcentrationProfileInMemory::Cursor2DImpl::getConcentrations()
{
    return this;
}


/* ************************************************************************** */
double BIO_IO_NS::ConcentrationProfileInMemory::Cursor2DImpl::getConcentration(int substanceNr)
{
    return source->matrix[currentH][currentV][substanceNr];
}


/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfileInMemory::Cursor2DImpl::setConcentration(int substanceNr, double concentration)
{
    source->matrix[currentH][currentV][substanceNr] = concentration;
}


/* ************************************************************************** */
/* ************************************************************************** */
