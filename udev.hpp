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

/* TODO:
 * - Use FOX framework functions to replace STL.
 **/

#pragma once

#include <libudev.h>
#include <unordered_map>
#include <string>
#include <iostream>
#include <iomanip>
#include <cassert>
#include <stdexcept>

struct DeviceProperty
{
    std::string node_path; // /dev/sdX
    std::string id_vendor;
    std::string id_product;
    std::string manufacturer;
    std::string product;
    std::string serial;
	std::string version;
	std::string max_power;
	std::string bus_num;
};


using MapProp = std::unordered_map<std::string, const DeviceProperty*>;


class Udev 
{
    public: // functions

        Udev();
        virtual ~Udev();
        int getMonitorEvent();
        void receiveEvents();
        void listDevices();
        const MapProp& getMapProperty() const;
        void initUdev();
   
    private: // functions

        bool isDisk(udev_device* dev) const;
        struct udev_device* isDiskUSB(udev_device* dev ) const;
        struct udev_device* isDiskATA(udev_device* dev ) const;
#ifdef DEBUG    
        void printInformation();
#endif
        void insertMapProperty(const DeviceProperty* dev_prop);
        void removeMapProperty(const std::string& key);
        const DeviceProperty* makeDeviceProperty(const std::string& node_path, udev_device* dev);

    private: // variables

		udev *m_udev;
        udev_monitor *m_monitor;
        MapProp m_map_prop;
};

/* vim: set ts=4 sw=4 tw=80 noet :*/
