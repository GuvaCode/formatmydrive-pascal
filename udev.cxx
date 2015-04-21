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


#include "udev.hpp"

#ifdef DEBUG
void Udev::printInformation()
{
    std::cout << "\n-- List property --" << std::endl;

    for (const auto &pair : m_map_prop)
    {
       const DeviceProperty* prop = pair.second; 

       std::cout << 
       "\nDevice path" << std::setw(4) << "\t: " << prop->node_path
       << "\nID_Vendor" << std::setw(4) << "\t: " << prop->id_vendor
       << "\nID_Product" << std::setw(4) << "\t: " << prop->id_product
       << "\nManufacturer" << std::setw(4) << "\t: " << prop->manufacturer
       << "\nProduct" << std::setw(4) << "\t: " << prop->product
       << "\nSerial" << std::setw(4) << "\t\t: " << prop->serial  
       << "\nVersion" << std::setw(4) << "\t: " << prop->version  
       << "\nPower max." << std::setw(4) << "\t: " << prop->max_power  
       << "\nBus" << std::setw(4) << "\t\t: " << prop->bus_num 
       << std::endl;
    }
}
#endif


bool Udev::isDisk(udev_device* dev) const
{
    const std::string stype (udev_device_get_devtype(dev) );
    return  ("disk" == stype) ? true : false;
}


/*
 * FIXME: esos param. no funcionan. 
 *  Da el mismo resultado que isDiskUSB.
udev_device* Udev::isDiskATA(udev_device* dev ) const
{
   return  udev_device_get_parent_with_subsystem_devtype(
           dev,
           "scsi",
           "disk");
}*/

udev_device* Udev::isDiskUSB(udev_device* dev ) const
{
   return  udev_device_get_parent_with_subsystem_devtype(
           dev,
           "usb",
           "usb_device");
}



/*
 * Enumera los disp. actualmente conectados
 */
void Udev::listDevices()
{
    assert(nullptr != m_udev);

	udev_enumerate* enumerate;
	udev_list_entry* devices, *dev_list_entry;

	enumerate = udev_enumerate_new(m_udev);
	udev_enumerate_add_match_subsystem(enumerate, "block");
	udev_enumerate_scan_devices(enumerate);
	devices = udev_enumerate_get_list_entry(enumerate);

	udev_list_entry_foreach(dev_list_entry, devices) 
    {
		const char* path = udev_list_entry_get_name(dev_list_entry);
		udev_device* dev = udev_device_new_from_syspath(m_udev, path);

        if (true == isDisk(dev))
        {
            const std::string node_path =  udev_device_get_devnode(dev);

            if (nullptr != (dev = isDiskUSB(dev)))
            {
                const DeviceProperty* prop = makeDeviceProperty(node_path, dev);
                insertMapProperty(prop);
#ifdef DEBUG
                printInformation();
#endif
            }
        }
        udev_device_unref(dev);
    }
	udev_enumerate_unref(enumerate);
}


void Udev::insertMapProperty(const DeviceProperty* dev_prop)
{
    m_map_prop[dev_prop->node_path] = dev_prop;
}


void Udev::removeMapProperty(const std::string& key)
{
    const DeviceProperty *prop = m_map_prop[key];
    
    assert(nullptr != prop); 

    if (nullptr != prop)
    {
        delete prop;
        m_map_prop.erase(key);
    }
}


const DeviceProperty* Udev::makeDeviceProperty(const std::string& node_path, udev_device* dev)
{
    DeviceProperty *dev_prop = new DeviceProperty { 
                    .node_path		= node_path,
                    .id_vendor		= udev_device_get_sysattr_value(dev, "idVendor"),
                    .id_product		= udev_device_get_sysattr_value(dev, "idProduct"),
                    .manufacturer	= udev_device_get_sysattr_value(dev, "manufacturer"), 
                    .product		= udev_device_get_sysattr_value(dev, "product"),
                    .serial			= "NN",
					.version		= udev_device_get_sysattr_value(dev, "version"),
					.max_power		= udev_device_get_sysattr_value(dev, "bMaxPower"),
					.bus_num		= udev_device_get_sysattr_value(dev, "busnum")
                };

    const char *serial = udev_device_get_sysattr_value(dev, "serial");

    if (nullptr != serial)
    {
        // Validar el serial
        for (const unsigned char *p = (unsigned char*)serial; *p != '\0'; p++)
        {
            if (*p < 0x20 || *p > 0x7f || *p == ',')
            {
                serial = "NN"; // serial no valido
                break;
            }
        }
        
        dev_prop->serial = serial;
    }

  return dev_prop;
}


/* 
 * Función notificada.
 * Eventos: add, remove
 *
 */
void Udev::receiveEvents()
{
    assert(nullptr != m_monitor);

    udev_device* dev = udev_monitor_receive_device(m_monitor);
    
    if (nullptr == dev) 
    { 
        const char *msg = { 
            "receiveEvents() : udev_udevice is null\n"
            "Impossible to receive any event.\n"
        };
        throw std::runtime_error(msg); 
    }

    if (false == isDisk(dev)) 
	{
		udev_device_unref(dev);
		return; 
	}
    
    const std::string node_path = udev_device_get_devnode(dev);
    const std::string action = udev_device_get_action(dev);
	udev_device* dev_usb = isDiskUSB(dev);

    if (nullptr != dev_usb)
    {
        if ("add" == action)
        {
            insertMapProperty( makeDeviceProperty(node_path, dev_usb) );
#ifdef DEBUG
            std::cout << "\nAction: " <<  action << std::endl;
            printInformation();
#endif
        }
        else if ("remove" == action)
        {
            removeMapProperty(node_path);
#ifdef DEBUG
            std::cout << "\nAction: " <<  action << std::endl;
            printInformation();
#endif
        }
    }
	udev_device_unref(dev);
}


int Udev::getMonitorEvent()
{
    assert(nullptr != m_udev);

	m_monitor = udev_monitor_new_from_netlink(m_udev, "udev");
	udev_monitor_filter_add_match_subsystem_devtype(m_monitor, "block", NULL);
	udev_monitor_enable_receiving(m_monitor);
	return udev_monitor_get_fd(m_monitor);
}


Udev::Udev() : m_udev(nullptr), m_monitor(nullptr)
{  }


void Udev::initUdev()
{
    m_udev = udev_new();
    
    if (nullptr == m_udev) 
    { 
        throw std::runtime_error("initUdev: Failed initialize udev\n"); 
    }
}


Udev::~Udev()
{
    udev_unref(m_udev);

    for (const auto &t : m_map_prop)
    {
        delete t.second;
    }

    m_map_prop.clear();
}

/* vim: set ts=4 sw=4 tw=80 noet :*/
