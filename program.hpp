/*
   Copyright (c) 2014, Daniel T. Borelli

   This file is part of FormatMyDrive.

    FormatMyDrive is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    FormatMyDrive is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with FormatMyDrive.  If not, see <http://www.gnu.org/licenses/>.
*/

#pragma once

#include <fx.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
#include <cerrno> //errno

#include "udev.hpp"

namespace Program  {

    static Udev udev;
}

