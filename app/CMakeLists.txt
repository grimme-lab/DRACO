# This file is part of DRACO.
# SPDX-Identifier: LGPL-3.0-or-later
#
# DRACO is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# DRACO is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with DRACO.  If not, see <https://www.gnu.org/licenses/>.

add_executable(
   "draco"
   "main.f90"
   )
target_link_libraries(
   "draco"
   PRIVATE
   "DRACO-lib"
  "mctc-lib::mctc-lib"
  "tblite::tblite"
  "numsa::numsa"
  ${BLAS_LIBRARIES}
  ${LAPACK_LIBRARIES}
   )

install(
   TARGETS
   "draco"
   DESTINATION
   "${CMAKE_INSTALL_BINDIR}"
   )
