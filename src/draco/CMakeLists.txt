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

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(APPEND srcs
   "${dir}/calc.f90"
   "${dir}/charges.f90"
   "${dir}/data.f90"
   "${dir}/fancy.f90"
   "${dir}/interface.f90"
   "${dir}/read.f90"
   "${dir}/type.f90"
   "${dir}/write.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)
