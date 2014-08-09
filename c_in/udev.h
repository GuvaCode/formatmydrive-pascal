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

#ifndef UDEV_CIN_H
#define UDEV_CIN_H

#include <stdio.h>
#include <stdlib.h>

#include <libudev.h>
#include <assert.h>
typedef struct 
{
    char node_path[41];
    char id_vendor[41];
    char id_product[41];
    char product[41];
    char manufacturer[41];
    char serial[41];
}DeviceProperty;

typedef void (*ListDevicesCallback)(void);

/* PRIVATE */

int isDisk(struct udev_device* dev);
struct udev_device* isDiskUSB(struct udev_device* dev );
void makeDeviceProperty();

static struct udev *m_udev;
static struct udev_device *m_device;
static struct udev_monitor *m_monitor;
static const char* m_node_path;
static DeviceProperty m_dev_prop;


/* EXTERN */

extern void InitializeUdev();
extern void UnitializeUdev();
extern int GetMonitorEvent();
extern const char* GetDeviceAttribute(const char* attr);
extern const char* GetDeviceNodePath();
extern int ReceiveEvents();
extern void ListDevices(ListDevicesCallback list_dev_cb);
extern const DeviceProperty* GetDeviceProperty();




#endif
