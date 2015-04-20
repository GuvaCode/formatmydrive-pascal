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

#include <unistd.h>

#include "window.hpp"

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */

FXDEFMAP( MainWindow ) MainWindowMap[] = {
    FXMAPFUNC(SEL_COMMAND,      MainWindow::ID_QUIT,        MainWindow::OnCmdQuit),
    FXMAPFUNC(SEL_SIGNAL,       MainWindow::ID_QUIT,        MainWindow::OnCmdQuit),
    FXMAPFUNC(SEL_CLOSE,        MainWindow::ID_TITLE,       MainWindow::OnCmdQuit),
    FXMAPFUNC(SEL_IO_READ,      MainWindow::ID_UDEV_EVENT,  MainWindow::OnCmdUdevEvent),
};

FXIMPLEMENT( MainWindow, FXMainWindow, MainWindowMap, ARRAYNUMBER(MainWindowMap) )

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */


MainWindow::MainWindow(FXApp* app) : FXMainWindow(app, "FormatMyDrive", NULL, NULL, DECOR_FIXSIZE, 0, 0, 393, 478)
{ 
    setTarget(this);
    setSelector(ID_TITLE);
}


MainWindow::MainWindow() 
{ }


MainWindow::~MainWindow()
{ }


void MainWindow::create()
{
	FXString title;
	title.format("FormatMyDrive v.%s", program::VERSION);

    FXMainWindow::create();
	setTitle(title);
    show(PLACEMENT_SCREEN);
}


long MainWindow::OnCmdQuit(FXObject* obj, FXSelector sel, void* data)
{
    getApp()->exit(0);
}


/* ID_UDEV_EVENT */
long MainWindow::OnCmdUdevEvent(FXObject* obj, FXSelector sel, void* data)
{
    Udev* udev = (Udev*)data;
    
    assert(nullptr != udev);

    if (nullptr != udev) 
    {  
        try 
        {
            udev->receiveEvents(); 
        }
        catch(const std::runtime_error& err)
        {
            // Error fatal.
            // F.M.D. depende de las notificaciones de eventos.
            std::cerr << "Error runtime: " << err.what() << std::endl;
			std::abort();
        }
    }

    return 1;
}

/* vim: set ts=4 sw=4 tw=80 noet :*/
