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
#ifndef BIO_DM_IComposite2D_HXX
#define BIO_DM_IComposite2D_HXX
#include "../../biosensor.hxx"
#include "../Splitted2DArea.hxx"
#include "IGrid2D.hxx"
#include "IGrid1D.hxx"
#include "IConcentrations.hxx"
BIO_DM_NS_BEGIN


typedef BIO_NS::Splitted2DArea<IGrid2D*, IGrid1D*, IConcentrations*> IComposite2D;


BIO_DM_NS_END
#endif
