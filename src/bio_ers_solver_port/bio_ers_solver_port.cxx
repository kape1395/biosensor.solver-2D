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
#include "ErlangMsgCodec.hxx"
#include "ErlangMsgCodec_config.hxx"
#include "ErlangMsgCodec_stop.hxx"

#define PORT_PACKET_SIZE 2
#define RC_UNDEFINED          10
#define RC_SIMULATION_DONE    11
#define RC_SIMULATION_STOPPED 12
#define RC_SIMULATION_FAILED  13

/**
 *  Entry point for the bio_ers_solver_port.
 */
int main(int argn, char **argv)
{
    std::ofstream log;
    log.open("bio_ers_solver_port.log", std::fstream::out | std::fstream::app);
    log << "main: Start" << std::endl;

    ErlangIO eio(std::cin, std::cout, PORT_PACKET_SIZE);
    eio.setLog(&log);

    ErlangMsgCodec_config codec_config;
    codec_config.setLog(&log);
    eio.addMessageCodec(&codec_config);

    ErlangMsgCodec_stop codec_stop;
    codec_stop.setLog(&log);
    eio.addMessageCodec(&codec_stop);

    log << "main: ErlangIO created" << std::endl;

    bool stop = false;
    int rc = RC_UNDEFINED;
    while (eio.live() && !stop)
    {
        log << "main: try to get message..." << std::endl;
        ErlangMsgCodec* msg = eio.getMessage(true);
        log << "main: got message " << msg << std::endl;
        if (!msg)
            continue;

        try
        {
            if (dynamic_cast<ErlangMsgCodec_config*>(msg))
            {
                ErlangMsgCodec_config* config = dynamic_cast<ErlangMsgCodec_config*>(msg);
                std::string modelStr = config->getModel();
                log << "main: received config message. Model is:\n" << modelStr << std::endl;
            }
            else if (dynamic_cast<ErlangMsgCodec_stop*>(msg))
            {
                log << "main: received stop message." << std::endl;
                stop = true;
                rc = RC_SIMULATION_STOPPED;
            }
            else
            {
                log << "main: received unknown message." << std::endl;
            }
        }
        catch (int error)
        {
            log << "main: error=" << error << " returned while decoding message." << std::endl;
        }
        eio.messageProcessed(msg);
    }


    log << "main: Stop" << std::endl;
    eio.setLog(0);
    log.close();
    return rc;
}

