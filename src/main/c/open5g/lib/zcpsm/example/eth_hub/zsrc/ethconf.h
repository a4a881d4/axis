#ifndef _ETH_CONF_H
#define _ETH_CONF_H


#define	LOCAL_MAC_0			0xaa
#define	LOCAL_MAC_1			0xaa
#define	LOCAL_MAC_2			0xaa
#define	LOCAL_MAC_3			0xaa
#define	LOCAL_MAC_4			0xaa
#define	LOCAL_MAC_5			0x00


#define	DST_MAC_0			0xaa
#define	DST_MAC_1			0xaa
#define	DST_MAC_2			0xaa
#define	DST_MAC_3			0xaa
#define	DST_MAC_4			0xaa
#define	DST_MAC_5			0xff


#define DATA_TYPE_0			0x08
#define	DATA_TYPE_1			0x0a
#define DEBUG_TYPE_0		0x08
#define	DEBUG_TYPE_1		0x0f
#define IO_TYPE_0				0x08
#define IO_TYPE_1				0x0b

#define LOCAL_IP_0			0x0A
#define LOCAL_IP_1			0x00
#define LOCAL_IP_2			0x43
#define LOCAL_IP_3			0x00

#define ETHRX_INFO_LENGTH 4
#define ETHTX_INFO_LENGTH	8
#define ETHTX_LOCALTIME_LENGTH 4
#define ETHTX_HEAD_LENGTH	24
#define ETHTX_HEAD_LENGTH_of_LASTFRAME	36
#define ETHTX_DB_HEAD_LENGTH 26

#endif
