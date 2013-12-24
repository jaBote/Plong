library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
 
entity controller is
    port(
        button_start: in std_logic;
        button_left: in std_logic;
        button_right: in std_logic;
        clk, not_reset: in std_logic;
        ctl_start, ctl_left, ctl_right: out std_logic
    );
end controller;
 
architecture arch of controller is
    constant DELAY: integer := 62500; -- 800Hz (clk period 20ns)
    signal counter, counter_next: std_logic_vector(15 downto 0);    
begin
    process(clk, not_reset)
    begin
        if not_reset = '0' then
            counter <= (others => '0');
        elsif falling_edge(clk) then
            counter <= counter_next;
        end if;
    end process;
 
    counter_next <= (counter + 1) when counter < DELAY else
                    (others => '0');
 
    -- outputs pulses of one clock cycle length at a rate of 800Hz
    ctl_start <= button_start when (counter = 0) else '0';
    ctl_left  <= button_left  when (counter = 0) else '0';
    ctl_right <= button_right when (counter = 0) else '0';
 
end arch;