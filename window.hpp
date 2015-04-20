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

#include "program.hpp"

class MainWindow : public FXMainWindow
{
    FXDECLARE(MainWindow)

    public: // functions

        MainWindow(FXApp*);
        MainWindow();
        virtual ~MainWindow();
        long OnCmdUdevEvent(FXObject* obj, FXSelector sel, void* data);
        long OnCmdQuit(FXObject* obj, FXSelector sel, void* data);

    public: // variables

        enum {
            ID_UDEV_EVENT = FXMainWindow::ID_LAST,
            ID_QUIT,
            ID_TITLE
        };

    private: // variables
    
        static constexpr auto DECOR_FIXSIZE = DECOR_TITLE | DECOR_MINIMIZE | DECOR_CLOSE;

    protected: // functions

        virtual void create();
  };

/* vim: set ts=4 sw=4 tw=80 noet :*/
