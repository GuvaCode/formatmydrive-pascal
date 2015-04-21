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


#include "program.hpp"
#include "window.hpp"

int main(int argc, char *argv[] )
{
    try  
    {
        program::udev.initUdev();
    } 
    catch(const std::runtime_error& err) 
    {
        std::cout << "Runtime error: " << err.what() <<  std::endl;
        return EXIT_FAILURE;
    }

    FXApp app("FormatMyDrive","fmtmydrv");
   
    app.init(argc, argv);
   
    MainWindow* mainWindow = new MainWindow(&app);

    app.create();

    app.addSignal(SIGINT, mainWindow, MainWindow::ID_QUIT);

	// Rule #1 Set up monitoring
    const FX::FXInputHandle fd_event = program::udev.getMonitorEvent();

	// Rule #2 Enumerate devices
	program::udev.listDevices();

	// Rule #3 Monitoring events
    app.addInput(mainWindow, MainWindow::ID_UDEV_EVENT, fd_event, INPUT_READ, &program::udev);

    return app.run();
}

/* vim: set ts=4 sw=4 tw=80 noet :*/
