#
# Copyright 2011 Karolis Petrauskas
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
pkglib_LTLIBRARIES = libbiosensor-slv-fd.la
libbiosensor_slv_fd_la_CPPFLAGS = @CPPFLAGS@ -I../libbiosensor-xml -I../libbiosensor
libbiosensor_slv_fd_la_LDFLAGS = @LDFLAGS@ -version-info 0:0:0
libbiosensor_slv_fd_la_LIBADD  = \
    ../libbiosensor/libbiosensor.la \
    ../libbiosensor-xml/libbiosensor-xml.la

libbiosensor_slv_fd_la_SOURCES = \
    bio/slv/fd/Factory.cxx \
    bio/slv/fd/FiniteDifferencesSolverAnalyzer.cxx \
    bio/slv/fd/ex2d/Solver.cxx \
    bio/slv/fd/im1d/Solver.cxx \
    bio/slv/fd/im1d/AreaSubSolver.cxx \
    bio/slv/fd/im1d/SubSolverFactory.cxx \
    bio/slv/fd/im2d/Solver.cxx \
    bio/slv/fd/im2d/DataModel.cxx \
    bio/slv/fd/im2d/AreaSubSolver.cxx \
    bio/slv/fd/im2d/BoundSubSolver.cxx \
    bio/slv/fd/im2d/CornerSubSolver.cxx \
    bio/slv/fd/im2d/ConstantCondition.cxx \
    bio/slv/fd/im2d/ConstantOnEdge.cxx \
    bio/slv/fd/im2d/FunctionSumOnEdge.cxx \
    bio/slv/fd/im2d/GradCondition.cxx \
    bio/slv/fd/im2d/SubstanceConcOnEdge.cxx \
    bio/slv/fd/im2d/SubstanceGradOnEdge.cxx \
    bio/slv/fd/im2d/SubSolverFactory.cxx \
    bio/slv/fd/im2d/MergeCondition.cxx \
    bio/slv/fd/im2d/TimeIntegralOnEdge.cxx \
    bio/slv/fd/im2d/WallCondition.cxx


nobase_pkginclude_HEADERS = \
    biosensor-slv-fd.hxx \
    bio/slv/fd/Factory.hxx

EXTRA_DIST = \
    bio/slv/fd/Factory.hxx \
    bio/slv/fd/FiniteDifferencesSolverAnalyzer.hxx \
    bio/slv/fd/ex2d/Solver.hxx \
    bio/slv/fd/im1d/Solver.hxx \
    bio/slv/fd/im1d/AreaSubSolver.hxx \
    bio/slv/fd/im1d/SubSolverFactory.hxx \
    bio/slv/fd/im2d/Solver.hxx \
    bio/slv/fd/im2d/DataModel.hxx \
    bio/slv/fd/im2d/AreaSubSolver.hxx \
    bio/slv/fd/im2d/BoundSubSolver.hxx \
    bio/slv/fd/im2d/CornerSubSolver.hxx \
    bio/slv/fd/im2d/GradCondition.hxx \
    bio/slv/fd/im2d/IAreaEdgeData.hxx \
    bio/slv/fd/im2d/IAreaEdgeFunction.hxx \
    bio/slv/fd/im2d/IBoundCondition.hxx \
    bio/slv/fd/im2d/ISubSolverFactory.hxx \
    bio/slv/fd/im2d/ConstantCondition.hxx \
    bio/slv/fd/im2d/ConstantOnEdge.hxx \
    bio/slv/fd/im2d/FunctionSumOnEdge.hxx \
    bio/slv/fd/im2d/SubstanceConcOnEdge.hxx \
    bio/slv/fd/im2d/SubstanceGradOnEdge.hxx \
    bio/slv/fd/im2d/SubSolverFactory.hxx \
    bio/slv/fd/im2d/MergeCondition.hxx \
    bio/slv/fd/im2d/ReactionMacro.hxx \
    bio/slv/fd/im2d/TimeIntegralOnEdge.hxx \
    bio/slv/fd/im2d/WallCondition.hxx


astyle:
	astyle @ASTYLE_OPTS@ *.hxx.in `find  bio -name  "*.cxx"` `find  bio -name  "*.hxx"`

