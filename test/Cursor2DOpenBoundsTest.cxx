#include <bio/dm/Cursor2DOpenBounds.hxx>
#include <bio/dm/Cursor2DOpenBounds.cxx> // To avoid using libraries.
#include <bio/dm/ICursor2D.hxx>
#include <iostream>
#include <string>
using namespace std;

/**
 * Mock implementation of a 2D Cursor.
 */
class MockCursor2D :
	public BIO_DM_NS::ICursor2D,
	public BIO_DM_NS::IConcentrations
{
private:
	double data[4][4];
    int posH;
    int posV;
public:
    MockCursor2D()
    {
    	int i = 0;
        for (int y = 0; y < 4; y++)
            for (int x = 0; x < 4; x++)
    			data[x][y] = i++;
        posH = 0;
        posV = 0;
    }
    virtual ~MockCursor2D(){}
    virtual void left()  { posH--; }
    virtual void right() { posH++; }
    virtual void top()   { posV--; }
    virtual void down()  { posV++; }
    virtual void rowStart() { posH = 0; }
    virtual void rowEnd()   { posH = 3; }
    virtual void colStart() { posV = 0; }
    virtual void colEnd()   { posV = 3; }
    virtual bool isValid()  { return posH >= 0 && posV >= 0 && posH < 4 && posV < 4; }
    virtual BIO_DM_NS::IConcentrations* getConcentrations() { return this; }
    virtual double getConcentration(int substanceNr) { return data[posH][posV]; };
    virtual void setConcentration(int substanceNr, double concentration) { };
};

void assertEquals(string name, double actual, double expected)
{
    if (actual != expected) {
        cout << name << " : expected " << expected << " but was " << actual << '\n';
        throw name;
    }
}
void assertTrue(string name, bool actual)
{
    if (!actual) {
        cout << name << " : expected true but was " << actual << '\n';
        throw name;
    }
}

/**
 * 00 01 02 03      05 05 06 06
 * 04 05 06 07      05 05 06 06
 * 08 09 10 11  ->  09 09 10 10
 * 12 13 14 15      09 09 10 10
 */
int main()
{
	MockCursor2D mock;
	BIO_DM_NS::Cursor2DOpenBounds instance(&mock, false);

	try {
                          assertEquals("0", instance.getConcentrations()->getConcentration(0), 5.0);
        instance.right(); assertEquals("1", instance.getConcentrations()->getConcentration(0), 5.0);
        instance.right(); assertEquals("2", instance.getConcentrations()->getConcentration(0), 6.0);
        instance.right(); assertEquals("3", instance.getConcentrations()->getConcentration(0), 6.0);
        instance.down(); assertEquals("4", instance.getConcentrations()->getConcentration(0), 6.0);
        instance.down(); assertEquals("5", instance.getConcentrations()->getConcentration(0), 10.0);
        instance.down(); assertEquals("6", instance.getConcentrations()->getConcentration(0), 10.0);
        instance.left(); assertEquals("7", instance.getConcentrations()->getConcentration(0), 10.0);
        instance.left(); assertEquals("8", instance.getConcentrations()->getConcentration(0), 9.0);
        instance.left(); assertEquals("9", instance.getConcentrations()->getConcentration(0), 9.0);
        instance.top(); assertEquals("10", instance.getConcentrations()->getConcentration(0), 9.0);
        instance.top(); assertEquals("11", instance.getConcentrations()->getConcentration(0), 5.0);
        instance.top(); assertEquals("12", instance.getConcentrations()->getConcentration(0), 5.0);
        instance.right(); assertEquals("13", instance.getConcentrations()->getConcentration(0), 5.0);
        instance.right(); assertEquals("14", instance.getConcentrations()->getConcentration(0), 6.0);

        instance.colEnd();
        instance.rowEnd();
        assertEquals("a", instance.getConcentrations()->getConcentration(0), 10.0);

        instance.colStart();
        instance.rowStart();
        assertEquals("b", instance.getConcentrations()->getConcentration(0), 5.0);

        instance.left();
        instance.left();
        instance.left();
        instance.left();
        assertEquals("c", instance.getConcentrations()->getConcentration(0), 5.0);
        assertTrue("x", !instance.isValid());

	} catch (...) {
	    cout << "Cursor2DOpenBoundsTest... Failed\n";
	    return 1;
	}
    cout << "Cursor2DOpenBoundsTest... OK\n";
    return 0;
}


