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


#include "udev.h"


int isDisk(struct udev_device* dev)
{
    const char* stype = udev_device_get_devtype(dev);
    return  ( 0 == strcmp(stype, "disk") ) ? 1 : 0;
}


struct udev_device* isDiskUSB(struct udev_device* dev )
{
   return  udev_device_get_parent_with_subsystem_devtype(
           dev,
           "usb",
           "usb_device");
}



/*
 * External
 * Enumera los disp. actualmente conectados
 */
void ListDevices(ListDevicesCallback list_dev_cb)
{
    assert(NULL != m_udev);
    assert(NULL != list_dev_cb);

	struct udev_enumerate* enumerate;
	struct udev_list_entry* devices, *dev_list_entry;

	enumerate = udev_enumerate_new(m_udev);
	udev_enumerate_add_match_subsystem(enumerate, "block");
	udev_enumerate_scan_devices(enumerate);
	devices = udev_enumerate_get_list_entry(enumerate);

	udev_list_entry_foreach(dev_list_entry, devices) 
    {
		const char* path = udev_list_entry_get_name(dev_list_entry);
	    struct udev_device* dev = udev_device_new_from_syspath(m_udev, path);

        if ( 1 == isDisk(dev) )
        {
            m_node_path =  udev_device_get_devnode(dev);

            if ( NULL != (dev = isDiskUSB(dev) ) )
            {
                m_device = dev;

                makeDeviceProperty();
                
                if ( list_dev_cb != NULL)
                {
                    (*list_dev_cb)();
                }
                else
                {
                    printf("\ncallback null\n");
                }
            }
        }
        udev_device_unref(dev);
    }

	udev_enumerate_unref(enumerate);
}


const DeviceProperty* GetDeviceProperty() 
{ 
    return &m_dev_prop; 
}


const char* GetDeviceAttribute(const char* attr)
{
    return  udev_device_get_sysattr_value(m_device, attr);
}


const char* GetDeviceNodePath() 
{ 
    return m_node_path; 
}


void makeDeviceProperty()
{
    strcpy(m_dev_prop.node_path, m_node_path);
    strcpy(m_dev_prop.id_vendor, udev_device_get_sysattr_value(m_device, "idVendor") );
    strcpy(m_dev_prop.id_product, udev_device_get_sysattr_value(m_device, "idProduct") );
    strcpy(m_dev_prop.product,  udev_device_get_sysattr_value(m_device, "product") );
    strcpy(m_dev_prop.manufacturer, udev_device_get_sysattr_value(m_device, "manufacturer") );
    strcpy(m_dev_prop.serial, udev_device_get_sysattr_value(m_device, "serial") );
}

/* 
 * External
 * Función notificada.
 * Eventos: add = 1, remove = 2
 *
 */
int ReceiveEvents()
{
    assert(NULL != m_monitor);

    struct udev_device* dev = udev_monitor_receive_device(m_monitor);
   
    if ( 0 == isDisk(dev) ) { return; }
    
    m_node_path =  udev_device_get_devnode(dev);
    const char * action = udev_device_get_action(dev);

    int ret = 0; // por defecto, ninguno.

    if ( NULL != (dev = isDiskUSB(dev) ) )
    {
        m_device = dev;

        if ( !strcmp(action, "add") )
        {
            ret = 1;
            makeDeviceProperty();
        }
        else if ( !strcmp(action, "remove") )
        {
            ret = 2;
            makeDeviceProperty();
        }

        /* Forzar vaciado.
         * Para freepascal.
         * */
        fflush(stdout);

    }
   	udev_device_unref(dev);

    return ret;
}


/*
 * External
 * */
int GetMonitorEvent() 
{
    assert(NULL != m_udev);

	m_monitor = udev_monitor_new_from_netlink(m_udev, "udev");
	udev_monitor_filter_add_match_subsystem_devtype(m_monitor, "block", NULL);
	udev_monitor_enable_receiving(m_monitor);
	return udev_monitor_get_fd(m_monitor);
}



void InitializeUdev() 
{
    m_udev = udev_new();
}

void UnitializeUdev()
{
    udev_unref(m_udev);
}

