// Copyright (C) 2016 Daniel Leitner and Andrea Schnepf. See //license.txt for details.

#include "RootSystem.h"
#include "analysis.h"

#include <iostream>
#include <fstream>
#include <unistd.h>


#include "examples/example1_wb_dgf.h"
#include "examples/example2.h"


/**
 * Starts an examples (from the examples folder)
 */
int main(int argc, char* argv[])
{
    string name="";

    if (argc>1) {
        name= argv[1];
    }
	
	//example2();

	example1_wb_dgf(); // open parameter file and output txt


//    if (argc>1) {
//        cout<<"starting simulation: "<< name <<"\n";
//        shehan_SoilCore(name, false); // put true here to export geometry
//    } else {
//        shehan_SoilCore(); // with default values
//    }
//
//     shehan_SoilCore("wheat",true);

    // shehan_RhizoTubes("wheat",true);

    // shehan_Trenches("wheat",true);

    // example_dumux(); // tests the suggested dumux coupling

    // example_exudation();

    return(0);

}



