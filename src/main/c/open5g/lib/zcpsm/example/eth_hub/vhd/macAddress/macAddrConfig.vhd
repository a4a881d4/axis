library ieee;
use ieee.std_logic_1164.all;

use work.eth_config.all;

entity macAddrConfig is
	port (
	ethtx_port_id		:	in	std_logic_vector(7 downto 0);
	ethrx_port_id		:	in	std_logic_vector(7 downto 0);
	db_port_id			:	in	std_logic_vector(7 downto 0);
	local_id_MAC0_Req	:	in	std_logic_vector(7 downto 0);
	local_id_MAC0_A		:	in	std_logic_vector(7 downto 0);	
	local_id_MAC0_B		:	in	std_logic_vector(7 downto 0);	
	local_id			:	in	std_logic_vector(39 downto 0);	
	ethtx_in_port 		: out std_logic_vector(7 downto 0);
	ethrx_in_port 		: out std_logic_vector(7 downto 0);
	db_in_port	 		: out std_logic_vector(7 downto 0)
	
	);
end macAddrConfig;

architecture behave of macAddrConfig is
begin

	ethtx_in_port <= 	local_id_MAC0_Req when ethtx_port_id = PORT_ETH_LOCAL_ID_0_REQ else
						local_id_MAC0_A when ethtx_port_id = PORT_ETH_LOCAL_ID_0_A else
						local_id_MAC0_B when ethtx_port_id = PORT_ETH_LOCAL_ID_0_B else
						local_id( 39 downto 32 ) when ethtx_port_id = PORT_ETH_LOCAL_ID_1 else
						local_id( 31 downto 24 ) when ethtx_port_id = PORT_ETH_LOCAL_ID_2 else
						local_id( 23 downto 16 ) when ethtx_port_id = PORT_ETH_LOCAL_ID_3 else
						local_id( 15 downto 8 ) when ethtx_port_id = PORT_ETH_LOCAL_ID_4 else
						local_id( 7 downto 0 ) when ethtx_port_id = PORT_ETH_LOCAL_ID_5 else
						(others => 'Z');
	ethrx_in_port <= 	local_id_MAC0_A when ethrx_port_id = PORT_ETH_LOCAL_ID_0_A else
						local_id_MAC0_B when ethrx_port_id = PORT_ETH_LOCAL_ID_0_B else
						local_id( 39 downto 32 ) when ethrx_port_id = PORT_ETH_LOCAL_ID_1 else
						local_id( 31 downto 24 ) when ethrx_port_id = PORT_ETH_LOCAL_ID_2 else
						local_id( 23 downto 16 ) when ethrx_port_id = PORT_ETH_LOCAL_ID_3 else
						local_id( 15 downto 8 ) when ethrx_port_id = PORT_ETH_LOCAL_ID_4 else
						local_id( 7 downto 0 ) when ethrx_port_id = PORT_ETH_LOCAL_ID_5 else
						(others => 'Z');		

	db_in_port <= 	local_id_MAC0_A when db_port_id = PORT_DB_LOCAL_ID_0_A else
					local_id_MAC0_B when db_port_id = PORT_DB_LOCAL_ID_0_B else
					local_id( 39 downto 32 ) when db_port_id = PORT_DB_LOCAL_ID_1 else
					local_id( 31 downto 24 ) when db_port_id = PORT_DB_LOCAL_ID_2 else
					local_id( 23 downto 16 ) when db_port_id = PORT_DB_LOCAL_ID_3 else
					local_id( 15 downto 8 ) when db_port_id = PORT_DB_LOCAL_ID_4 else
					local_id( 7 downto 0 ) when db_port_id = PORT_DB_LOCAL_ID_5 else
					(others => 'Z');
end behave;

			