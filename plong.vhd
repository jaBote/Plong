library ieee;
use ieee.std_logic_1164.all;

entity plong is
    port (
        clk, not_reset: in std_logic;
        hsync, vsync: out  std_logic;
        rgb: out std_logic_vector(2 downto 0);
        speaker: out std_logic
    );
end plong;

architecture arch of plong is
    signal rgb_reg, rgb_next: std_logic_vector(2 downto 0);
    signal video_on: std_logic;
    signal px_x, px_y: std_logic_vector(9 downto 0);

    signal ball_bounced, ball_missed: std_logic;

    signal ctl_up, ctl_down: std_logic;

begin
    process (clk, not_reset)
    begin
        if clk'event and clk = '0' then
            rgb_reg <= rgb_next;
        end if;
    end process;

    -- instantiate VGA Synchronization circuit
    vga_sync_unit:
        entity work.vga(sync)
        port map(
            clk => clk, not_reset => not_reset,
            hsync => hsync, vsync => vsync,
            video_on => video_on,
            pixel_x => px_x, pixel_y => px_y
        );

    graphics_unit:
        entity work.graphics(dispatcher)
        port map(
            clk => clk, not_reset => not_reset,
            ctl_up => ctl_up, ctl_down => ctl_down,
            px_x => px_x, px_y => px_y,
            video_on => video_on,
            rgb_stream => rgb_next,
            ball_bounced => ball_bounced,
            ball_missed => ball_missed
        );

    sound:
        entity work.player(behaviour)
        port map(
            clk => clk, not_reset => not_reset,
            bump_sound => ball_bounced, miss_sound => ball_missed,
            speaker => speaker
        );

    rgb <= rgb_reg;
end arch;
