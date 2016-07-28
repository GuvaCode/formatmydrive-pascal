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

/*
 * Private
 * */
int isDisk(struct udev_device* dev)
{
    const char* stype = udev_device_get_devtype(dev);
    return  ( 0 == strcmp(stype, "disk") ) ? 1 : 0;
}

/*
 * Private
 * */
struct udev_device* isDiskUSB(struct udev_device* dev )
{
   struct udev_device * parent;
   parent = udev_device_get_parent_with_subsystem_devtype(
           dev,
           "usb",
           "usb_device");
   return parent;
}

/*
 * External
 * Enumera los disp. actualmente conectados.
 */
void ListDevices(ListDevicesCallback list_dev_cb)
{
    assert(NULL != m_udev);
    assert(NULL != list_dev_cb);

	struct udev_enumerate* enumerate;
	struct udev_list_entry* devices, *dev_list_entry;
    struct udev_device * parent;


	enumerate = udev_enumerate_new(m_udev);
	udev_enumerate_add_match_subsystem(enumerate, "block");
	udev_enumerate_scan_devices(enumerate);
	devices = udev_enumerate_get_list_entry(enumerate);

    //loop
	udev_list_entry_foreach(dev_list_entry, devices) 
    {
		const char* path = udev_list_entry_get_name(dev_list_entry);
	    struct udev_device* dev = udev_device_new_from_syspath(m_udev, path);

        if ( 1 == isDisk(dev) )
        {
            m_node_path =  udev_device_get_devnode(dev);

            if ( NULL != (parent = isDiskUSB(dev) ) )
            {
                m_device = parent;

                makeDeviceProperty();
                
                assert(NULL != list_dev_cb );

                // call pascal func.
                (*list_dev_cb)();
            }
        }
        udev_device_unref(dev);
    }
	udev_enumerate_unref(enumerate);
}

/*
 * External
 * */
const DeviceProperty* const GetDeviceProperty() 
{ 
    return &m_dev_prop; 
}

/*
 * External
 * */
const char* GetDeviceAttribute(const char* attr)
{
    return  udev_device_get_sysattr_value(m_device, attr);
}

/*
 * Private
 * */
void makeDeviceProperty()
{
    strncpy(m_dev_prop.node_path,    m_node_path, 40);
    strncpy(m_dev_prop.id_vendor,    GetDeviceAttribute("idVendor"), 40);
    strncpy(m_dev_prop.id_product,   GetDeviceAttribute("idProduct"), 40);
    strncpy(m_dev_prop.product,      GetDeviceAttribute("product"), 40);
    strncpy(m_dev_prop.manufacturer, GetDeviceAttribute("manufacturer"), 40);
    strncpy(m_dev_prop.version,      GetDeviceAttribute("version"), 40);
    strncpy(m_dev_prop.max_power,    GetDeviceAttribute("bMaxPower"), 40);
    strncpy(m_dev_prop.bus,          GetDeviceAttribute("busnum"), 40);

    const char * serial = GetDeviceAttribute("serial");

    if (serial != NULL)
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
        
    }

    strcpy(m_dev_prop.serial, serial);
}

/* 
 * External
 * Función notificada.
 * */
int32_t ReceiveEvents()
{
    assert(NULL != m_monitor);

    int32_t ret = RECEVENT_NONE;

    struct udev_device* dev = udev_monitor_receive_device(m_monitor);
   
    if ( 0 == isDisk(dev) ) 
    { 
        udev_device_unref (dev);
        return ret; 
    }
    
    struct udev_device * parent;

    m_node_path =  udev_device_get_devnode(dev);

    const char * action = udev_device_get_action(dev);

    if ( NULL != (parent = isDiskUSB(dev) ) )
    {
        m_device = parent;

        if ( !strcmp(action, "add") )
        {
            ret = RECEVENT_ADD;
            makeDeviceProperty();
        }
        else if ( !strcmp(action, "remove") )
        {
            ret = RECEVENT_REMOVE;
            makeDeviceProperty();
        }
    }
   	udev_device_unref(dev);

    return ret;
}

/*
 * External
 * Retorna un 'file descriptor' para utilizarlo
 * como notificador.
 * */
int32_t GetMonitorEvent() 
{
    assert(NULL != m_udev);

	m_monitor = udev_monitor_new_from_netlink(m_udev, "udev");
	udev_monitor_filter_add_match_subsystem_devtype(m_monitor, "block", NULL);
	udev_monitor_enable_receiving(m_monitor);
	return udev_monitor_get_fd(m_monitor);
}

/*
 * External
 * */
void InitializeUdev() 
{
    m_udev = udev_new();
    assert(NULL != m_udev);
}

/*
 * External
 * */
void UnitializeUdev()
{
    udev_unref(m_udev);
}

