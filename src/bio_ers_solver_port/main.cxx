/*
 * Copyright 2012 Karolis Petrauskas
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
#include <iostream>
#include <fstream>
#include <ei.h>
#include <erl_interface.h>
#include "ErlangIO.hxx"


/**
 *  Entry point for the bio_ers_solver_port.
 */
int main(int argn, char **argv)
{
    std::ofstream log;
    log.open("bio_ers_solver_port.log", std::fstream::out);

    ErlangIO eio(std::cin, std::cout, 2);
    eio.setLog(&log);

    while (eio.live())
    {
        eio.test();
    }

    eio.setLog(0);
    log.close();
    return 0;
}

